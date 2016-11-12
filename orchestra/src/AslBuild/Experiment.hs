{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiment
    ( generateTargetFor
    , genExperimentSetup
    , genClientSetup
    , genMiddleSetup
    , genServerSetups
    , getVmsForExperiments
    , resultSummariesLocationFile
    , readResultsSummaryLocationsForCfg
    , readResultsSummaryLocations
    , readResultsSummary
    , readExperimentSetup
    , readClientResults
    , experimentLocalTmpDir
    , module AslBuild.Experiment.Types
    ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List                  (intercalate)

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.BuildMemcached
import           AslBuild.Client
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiment.Types
import           AslBuild.Jar
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
        need [memcachedBin, memaslapBin, outputJarFile]
        need [provisionLocalhostRule]

        (eSetups, vmsNeeded) <- genExperimentSetups ecf

        provisionVmsFromData vmsNeeded

        -- Intentionally no parallelism here.
        forM_ (indexed eSetups) $ \(ix, es) -> do
            printBanner ecf ix eSetups
            runOneExperiment es

        writeJSON rFile $ map esResultsSummaryFile eSetups

startTime :: Int
startTime = 1

serverStartTime :: Int
serverStartTime = startTime

middleStartTime :: Int
middleStartTime = 2 * startTime

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

runOneExperiment :: ExperimentSetup -> Action ()
runOneExperiment es@ExperimentSetup{..} = do
    -- Get the clients configs set up
    setupClientConfigs clientSetups

    -- Start the servers
    startServersOn serverSetups

    -- Wait for the servers to get started
    wait serverStartTime

    -- Start the middleware
    middlePh <- startMiddleOn middleSetup

    -- Wait for the middleware to get started
    wait middleStartTime

    -- Start the clients
    clientPhs <- startClientsOn clientSetups

    -- Wait for the experiment to finish
    actionFinally (waitNicely $ toSeconds esRuntime) (return ())

    putLoud "Done waiting for the runtime, now just waiting for the clients to finish."

    -- Wait for memaslap to stop running. (This should not be long now, but who knows.)
    waitForClients clientPhs

    -- Shut down the middleware
    shutdownMiddle middleSetup middlePh

    -- Shut down the servers
    shutdownServers serverSetups

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
            { erClientResultsFiles = map cResultsFile clientSetups
            , erMiddleResultsFile = mLocalTrace middleSetup
            , erSetupFile = esSetupFile
            }

    -- Write the result record to file
    writeJSON esResultsSummaryFile results

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
makeClientResultFile cSetup@ClientSetup{..} = do
    mel <- parseLog cLocalLog
    case mel of
        Nothing -> fail $ "could not parse logfile: " ++ cLocalLog
        Just parsedLog -> do
            let results = ClientResults
                    { crSetup = cSetup
                    , crLog = parsedLog
                    }
            writeJSON cResultsFile results

waitNicely :: Int -> Action ()
waitNicely is = do
    putLoud $ "Waiting for " ++ toClockString is
    go is
  where
    period = 10
    go s = do
        putLoud $ toClockString s ++ " remaining."
        if s < period
        then liftIO $ threadDelay $ s * 1000 * 1000
        else do
            liftIO $ threadDelay $ period * 1000 * 1000
            go $ s - period

getVmsForExperiments
    :: ExperimentConfig a
    => a
    -> Action ([(RemoteLogin, String)], [(RemoteLogin, String)], [(RemoteLogin, String)], [VmData])
getVmsForExperiments ecf = do
    let HighLevelConfig{..} = highLevelConfig ecf
    case location of
        Local -> do
            let localLogin = RemoteLogin Nothing localhostIp
            let localPrivate = localhostIp
            let localTup = (localLogin, localPrivate)
            let locals = repeat localTup
            return (take nrClients locals, take 1 locals, take nrServers locals, [])
        Remote -> do
            (cs, ms, ss) <- getVms nrClients 1 nrServers
            let login VmData{..} = RemoteLogin (Just vmAdmin) vmFullUrl
            let private VmData{..} = vmPrivateIp
            let tups = map (login &&& private)
            return (tups cs, tups ms, tups ss, cs ++ ms ++ ss)

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
    , mLocalTrace = experimentResultsDir ecf </> "traces" </> traceFileName <.> csvExt
    , mMiddlewareFlags = MiddlewareFlags
        { mwIp = mPrivate
        , mwPort = middlePort
        , mwNrThreads = 1
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

genClientSetup
    :: ExperimentConfig a
    => a
    -> [(RemoteLogin, String)]
    -> MiddleSetup
    -> (String -> FilePath)
    -> TimeUnit
    -> [ClientSetup]
genClientSetup ecf cls middle signGlobally runtime = flip map (indexed cls) $ \(cix, (cLogin, _)) ->
    let target = experimentTarget ecf
        sign f = signGlobally $ intercalate "-" [target, show cix, f]
    in ClientSetup
        { cRemoteLogin = cLogin
        , cIndex = cix
        , cLocalLog
            = experimentLocalTmpDir ecf </> "local-client-logs" </> sign "client-local-log"
        , cRemoteLog
            = experimentRemoteTmpDir ecf </> sign "memaslap-remote-log"
        , cResultsFile
            = experimentResultsDir ecf </> "client-results" </> sign "client-results"
        , cLocalMemaslapConfigFile
            = experimentLocalTmpDir ecf </> "memaslap-configs" </> sign "memaslap-config"
        , cMemaslapSettings = MemaslapSettings
            { msConfig = defaultMemaslapConfig
                { setProportion = 0.05
                }
            , msFlags = MemaslapFlags
                { msServers = [middleRemoteServer middle]
                , msThreads = 1
                , msConcurrency = 64
                , msOverwrite = 0.9
                , msStatFreq = Just $ Seconds 1
                , msWorkload = WorkFor runtime
                , msWindowSize = Kilo 1
                , msConfigFile = experimentRemoteTmpDir ecf </> sign "memaslapcfg"
                }
            }
        }


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
        = experimentResultsDir ecf </> "summaries" </> signGlobally "summary" <.> jsonExt
    , esSetupFile
        = experimentResultsDir ecf </> "setups" </> signGlobally "setup" <.> jsonExt
    , clientSetups = clients
    , middleSetup = middle
    , serverSetups = servers
    }

readResultsSummaryLocationsForCfg :: (MonadIO m, ExperimentConfig a) => a -> m [FilePath]
readResultsSummaryLocationsForCfg = readResultsSummaryLocations . resultSummariesLocationFile

readResultsSummaryLocations :: MonadIO m => FilePath -> m [FilePath]
readResultsSummaryLocations = readJSON

readResultsSummary :: MonadIO m => FilePath -> m ExperimentResultSummary
readResultsSummary = readJSON

readExperimentSetup :: MonadIO m => FilePath -> m ExperimentSetup
readExperimentSetup = readJSON

readClientResults :: MonadIO m => FilePath -> m ClientResults
readClientResults = readJSON
