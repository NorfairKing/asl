{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiment
    ( generateTargetFor
    , defaultConcurrency
    , defaultMiddleThreads
    , genExperimentSetup
    , genClientSetup
    , genMiddleSetup
    , genServerSetups
    , getVmsForExperiments
    , localClientResultsFile
    , localClientLogDir
    , localMiddleTraceDir
    , resultSummariesLocationFile
    , readResultsSummaryLocationsForCfg
    , readResultsSummaryLocations
    , readResultsSummary
    , readExperimentSetupForSummary
    , readExperimentSetup
    , readClientResults
    , experimentResultsDir
    , experimentLocalTmpDir
    , module AslBuild.Experiment.Types
    ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                  (intercalate)

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Client
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiment.Types
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Provision
import           AslBuild.Server
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm.Data
import           AslBuild.Vm.Types

generateTargetFor
    :: ExperimentConfig a
    => a
    -> Rules ()
generateTargetFor ecf = do
    let rFile = resultSummariesLocationFile ecf

    experimentTarget ecf ~> need [rFile]

    rFile %> \_ -> do
        need [provisionLocalhostRule]

        (eSetups, vmsNeeded) <- genExperimentSetups ecf

        provisionVms vmsNeeded
        runExperiments ecf eSetups vmsNeeded

        writeJSON rFile $ map esResultsSummaryFile eSetups

retryPolicy :: Int
retryPolicy = 3

runExperiments :: ExperimentConfig a => a -> [ExperimentSetup] -> [RemoteLogin] -> Action ()
runExperiments ecf eSetups vmsNeeded =
    -- Intentionally no parallelism here.
    forM_ (indexed eSetups) $ \(ix, es) -> do
        let resSumFile = esResultsSummaryFile es
        alreadyDone <- doesFileExist resSumFile
        if alreadyDone
        then do
            putLoud $ unwords
                [ "Not rerunning experiment"
                , show ix
                , "because its results file already exists:"
                ]
            putLoud resSumFile
        else do
            printBanner ecf ix eSetups
            retryAsNeeded es
  where
    retryAsNeeded es = go 1
      where
        go i
            | i > retryPolicy = fail $ unwords
                [ "Experiment failed "
                , show retryPolicy
                , "times. Stopping here."
                ]
            | otherwise = do
                result <- runOneExperiment es
                case result of
                    ExperimentSuccess -> pure () -- Done
                    ExperimentFailure err -> do
                        putLoud $ unwords
                            [ "Experiment failed in attempt"
                            , show i
                            , "with the following error, "
                            , "trying again."
                            ]
                        putLoud err
                        putLoud "Clearing all processes."
                        clearLocal
                        clearVms vmsNeeded
                        putLoud "Waiting a while to let everything settle down."
                        wait 5
                        go $ i + 1


startTime :: Int
startTime = 1

serverStartTime :: Int
serverStartTime = 2 * startTime

middleStartTime :: Int
middleStartTime = 5 * startTime

shutdownTime :: Int
shutdownTime = 1

guessedOverheadTime :: Int
guessedOverheadTime = 2

totalRuntimeRemaining :: Int -> [ExperimentSetup] -> Int
totalRuntimeRemaining ix eSetups = sum
    $ map (\ExperimentSetup{..} -> sum
        [ toSeconds esRuntime
        , serverStartTime
        , middleStartTime
        , shutdownTime
        , guessedOverheadTime
        ])
    $ drop ix eSetups

printBanner :: ExperimentConfig a => a -> Int -> [ExperimentSetup] -> Action ()
printBanner ecf ix eSetups = do
    let nrOfExperiments = length eSetups
    let statusStr = unwords
            [ "|===["
            , "Running experiment:"
            , experimentTarget ecf
            , concat ["[", show (ix + 1), "/", show nrOfExperiments, "]"]
            , "]===|"
            ]
    let upBannerStr = "/" ++ replicate (length statusStr - 2) '-' ++ "\\"
    let doBannerStr = "\\" ++ replicate (length statusStr - 2) '-' ++ "/"
    putLoud upBannerStr
    putLoud statusStr
    putLoud doBannerStr

    putLoud $ unwords
        [ "Approximately"
        , toClockString $ totalRuntimeRemaining ix eSetups
        , "remaining."
        ]

runOneExperiment :: ExperimentSetup -> Action ExperimentSuccess
runOneExperiment es@ExperimentSetup{..} = do
    -- Get the clients configs set up
    setupClientConfigs clientSetups

    let serverSetups = case backendSetup of
            Left serverSetup -> [serverSetup]
            Right (_, ss) -> ss

    -- Start the servers
    startServersOn serverSetups

    -- Wait for the servers to get started
    wait serverStartTime

    mMiddle <- case backendSetup of
        Left _ -> return Nothing
        Right (middleSetup, _) -> do
            -- Start the middleware
            middlePh <- startMiddleOn middleSetup

            -- Wait for the middleware to get started
            wait middleStartTime
            return $ Just (middleSetup, middlePh)

    -- Start the clients
    clientPhs <- startClientsOn clientSetups

    -- Wait for the experiment to finish
    -- Wait for memaslap to stop running. (This should not be long now, but who knows.)
    alreadyFailed <- waitAndWaitForClients esRuntime clientPhs
    case alreadyFailed of
        ExperimentFailure _ -> return alreadyFailed
        _ -> do
            case mMiddle of
                Nothing -> return ()
                Just (middleSetup, middlePh) ->
                    -- Shut down the middleware
                    shutdownMiddle middleSetup middlePh

            -- Shut down the servers
            shutdownServers serverSetups

            case mMiddle of
                Nothing -> return ()
                Just (middleSetup, _) ->
                    -- Copy the middleware logs back
                    copyMiddleTraceBack middleSetup

            -- Shut down the memaslap instances
            shutdownClients clientSetups

            -- Copy the client logs back
            copyClientLogsBack clientSetups

            -- Prepare analysis files for the client logs.
            makeClientResultFiles clientSetups

            -- Write the setup file
            writeJSON esSetupFile es

            -- Make the result record
            let results = ExperimentResultSummary
                    { erClientLogFiles = map cLocalLog clientSetups
                    , merMiddleResultsFile = (mLocalTrace . fst) <$> mMiddle
                    , erSetupFile = esSetupFile
                    }

            -- Write the result record to file
            writeJSON esResultsSummaryFile results

            return ExperimentSuccess

experimentResultsDir
    :: ExperimentConfig a
    => a -> FilePath
experimentResultsDir a = case resultsPersistence $ highLevelConfig a of
    Persistent -> resultsDir </> experimentTarget a
    Volatile -> tmpDir </> experimentTarget a

experimentLocalTmpDir
    :: ExperimentConfig a
    => a -> FilePath
experimentLocalTmpDir a = tmpDir </> experimentTarget a

experimentRemoteTmpDir
    :: ExperimentConfig a
    => a -> FilePath
experimentRemoteTmpDir a = remoteTmpDir </> experimentTarget a

resultSummariesLocationFile :: ExperimentConfig a => a -> FilePath
resultSummariesLocationFile cfg
    = experimentResultsDir cfg </> "summary-locations" <.> jsonExt

makeClientResultFiles :: [ClientSetup] -> Action ()
makeClientResultFiles = (`forP_` makeClientResultFile)

makeClientResultFile :: ClientSetup -> Action ()
makeClientResultFile ClientSetup{..} = do
    mel <- parseLog cLocalLog
    case mel of
        Nothing -> fail $ "could not parse logfile: " ++ cLocalLog
        Just _ -> return ()

getVmsForExperiments
    :: ExperimentConfig a
    => a
    -> Bool
    -> Action ([(RemoteLogin, String)], [(RemoteLogin, String)], [(RemoteLogin, String)], [RemoteLogin])
getVmsForExperiments ecf useMiddle = do
    let HighLevelConfig{..} = highLevelConfig ecf
    let nrMiddles = if useMiddle then 1 else 0
    case location of
        Local -> do
            let localLogin = RemoteLogin Nothing localhostIp
            let localPrivate = localhostIp
            let localTup = (localLogin, localPrivate)
            let locals = repeat localTup
            return (take nrClients locals, take nrMiddles locals, take nrServers locals, [])
        Remote -> do
            (cs, ms, ss) <- getVms nrClients nrMiddles nrServers
            let login VmData{..} = RemoteLogin (Just vmAdmin) vmFullUrl
            let private VmData{..} = vmPrivateIp
            let tups = map (login &&& private)
            return (tups cs, tups ms, tups ss, map login $ cs ++ ms ++ ss)

genServerSetups :: [(RemoteLogin, String)] -> [ServerSetup]
genServerSetups sers = flip map (indexed sers) $ \(six, (sLogin, _)) -> ServerSetup
    { sRemoteLogin = sLogin
    , sIndex = six
    , sMemcachedFlags = MemcachedFlags
        { memcachedPort = serverPort + six
        , memcachedAsDaemon = True
        }
    }
  where serverPort = 12345

localMiddleTraceDir :: ExperimentConfig a => a -> FilePath
localMiddleTraceDir ecf = experimentResultsDir ecf </> "traces"

genMiddleSetup
    :: ExperimentConfig a
    => a
    -> (RemoteLogin, String)
    -> [ServerSetup]
    -> [(RemoteLogin, String)]
    -> (String -> FilePath)
    -> MiddleSetup
genMiddleSetup ecf (mLogin, mPrivate) servers sers signGlobally = MiddleSetup
    { mRemoteLogin = mLogin
    , mLocalTrace = localMiddleTraceDir ecf </> traceFileName <.> csvExt
    , mMiddlewareFlags = MiddlewareFlags
        { mwIp = mPrivate
        , mwPort = middlePort
        , mwNrThreads = defaultMiddleThreads
        , mwReplicationFactor = length servers
        , mwServers = map
            (\(ServerSetup{..}, (_, sPrivate)) ->
                RemoteServerUrl
                    sPrivate
                    (memcachedPort sMemcachedFlags))
            (zip servers sers)
        , mwTraceFile = experimentRemoteTmpDir ecf </> traceFileName <.> csvExt
        , mwVerbosity = LogOff
        , mwReadSampleRate = Just 1000
        , mwWriteSampleRate = Just 1000
        }
    }
  where
    target = experimentTarget ecf
    middlePort = 23456
    traceFileName = signGlobally (target ++ "-trace")

localClientLogDir :: ExperimentConfig a => a -> FilePath
localClientLogDir ecf = experimentResultsDir ecf </> "local-client-logs"

localClientResultsDir :: ExperimentConfig a => a -> FilePath
localClientResultsDir ecf = experimentLocalTmpDir ecf </> "client-results"

-- The results file for a given log file.
localClientResultsFile :: ExperimentConfig a => a -> FilePath -> FilePath
localClientResultsFile ecf fp = (fp `replaceDirectory` localClientResultsDir ecf) ++ "-results" <.> jsonExt

genClientSetup
    :: (ExperimentConfig a)
    => a
    -> [(RemoteLogin, String)]
    -> RemoteServerUrl
    -> (String -> FilePath)
    -> TimeUnit
    -> [ClientSetup]
genClientSetup ecf cls surl signGlobally runtime = flip map (indexed cls) $ \(cix, (cLogin, _)) ->
    let target = experimentTarget ecf
        sign f = signGlobally $ intercalate "-" [target, show cix, f]
    in ClientSetup
        { cRemoteLogin = cLogin
        , cIndex = cix
        , cLocalLog
            = localClientLogDir ecf </> sign "client-local-log"
        , cRemoteLog
            = experimentRemoteTmpDir ecf </> sign "memaslap-remote-log"
        , cLocalMemaslapConfigFile
            = experimentLocalTmpDir ecf </> "memaslap-configs" </> sign "memaslap-config"
        , cMemaslapSettings = MemaslapSettings
            { msConfig = defaultMemaslapConfig
                { setProportion = 0.05
                }
            , msFlags = MemaslapFlags
                { msServers = [surl]
                , msThreads = 1
                , msConcurrency = defaultConcurrency
                , msOverwrite = 0.9
                , msStatFreq = Just $ Seconds 1
                , msWorkload = WorkFor runtime
                , msWindowSize = Kilo 1
                , msConfigFile = experimentRemoteTmpDir ecf </> sign "memaslapcfg"
                }
            }
        }

defaultConcurrency :: Int
defaultConcurrency = 35

defaultMiddleThreads :: Int
defaultMiddleThreads = 16

localExperimentSummariesDir :: ExperimentConfig a => a -> FilePath
localExperimentSummariesDir ecf = experimentResultsDir ecf </> "summaries"

localExperimentSetupsDir :: ExperimentConfig a => a -> FilePath
localExperimentSetupsDir ecf = experimentResultsDir ecf </> "setups"

genExperimentSetup
    :: ExperimentConfig a
    => a
    -> TimeUnit
    -> [ClientSetup]
    -> MiddleSetup
    -> [ServerSetup]
    -> (String -> FilePath)
    -> ExperimentSetup
genExperimentSetup ecf runtime clients middle servers signGlobally = ExperimentSetup
    { esRuntime = runtime
    , esResultsSummaryFile
        = localExperimentSummariesDir ecf </> signGlobally "summary" <.> jsonExt
    , esSetupFile
        = localExperimentSetupsDir ecf </> signGlobally "setup" <.> jsonExt
    , clientSetups = clients
    , backendSetup = Right (middle, servers)
    }

readResultsSummaryLocationsForCfg :: (MonadIO m, ExperimentConfig a) => a -> m [FilePath]
readResultsSummaryLocationsForCfg = readResultsSummaryLocations . resultSummariesLocationFile

readResultsSummaryLocations :: MonadIO m => FilePath -> m [FilePath]
readResultsSummaryLocations = readJSON

{-# DEPRECATED #-}
readResultsSummary :: MonadIO m => FilePath -> m ExperimentResultSummary
readResultsSummary = readJSON

readExperimentSetupForSummary :: MonadIO m => ExperimentResultSummary -> m ExperimentSetup
readExperimentSetupForSummary = readExperimentSetup . erSetupFile

{-# DEPRECATED #-}
readExperimentSetup :: MonadIO m => FilePath -> m ExperimentSetup
readExperimentSetup = readJSON

readClientResults :: MonadIO m => FilePath -> m MemaslapLog
readClientResults = readJSON
