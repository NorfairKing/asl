{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiment
    ( module AslBuild.Experiment
    , module AslBuild.Experiment.Types
    ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Data.List                  (intercalate)
import           System.Process

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
import           AslBuild.Vm

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

        forM_ eSetups $ \ExperimentSetup{..} -> do
            -- Get the clients configs set up
            setupClientConfigs clientSetups

            -- Start the servers
            startServersOn serverSetups

            -- Wait for the servers to get started
            wait 1

            -- Start the middleware
            middlePh <- startMiddleOn middleSetup

            -- Wait for the middleware to get started
            wait 1

            -- Start the clients
            startClientsOn clientSetups

            -- Wait for the experiment to finish
            actionFinally (waitNicely $ toSeconds esRuntime) (return ())

            -- Shut down the middleware
            shutdownMiddle middleSetup
            void $ liftIO $ waitForProcess middlePh

            -- Shut down the servers
            shutdownServers serverSetups

            -- Copy the middleware logs back
            copyMiddleTraceBack middleSetup

            -- Wait for memaslap to finish writing logs.
            wait 1

            -- Shut down the memaslap instances
            shutdownClients clientSetups

            -- Copy the client logs back
            copyClientLogsBack clientSetups

            -- Prepare analysis files for the client logs.
            makeClientResultFiles clientSetups

            -- Make the result record
            let results = ExperimentResultSummary
                    { erClientResults = map cResultsFile clientSetups
                    , erMiddleResults = mLocalTrace middleSetup
                    }

            -- Write the result record to file
            writeJSON esResultsSummaryFile results

        writeJSON rFile $ map esResultsSummaryFile eSetups

experimentResultsDir
    :: ExperimentConfig a
    => a -> FilePath
experimentResultsDir a = resultsDir </> experimentTarget a

experimentLocalTmpDir
    :: ExperimentConfig a
    => a -> FilePath
experimentLocalTmpDir a = tmpDir </> experimentTarget a

experimentRemoteTmpDir
    :: ExperimentConfig a
    => a -> FilePath
experimentRemoteTmpDir a = "/tmp" </> experimentTarget a

resultSummariesLocationFile :: ExperimentConfig a => a -> FilePath
resultSummariesLocationFile cfg
    = resultsDir </> experimentTarget cfg ++ "-summary-locations" <.> jsonExt

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
    , mLocalTrace = experimentResultsDir ecf </> traceFileName <.> csvExt
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
        , cLocalLog = experimentLocalTmpDir ecf </> sign "client-local-log"
        , cRemoteLog = experimentRemoteTmpDir ecf </> sign "memaslap-remote-log"
        , cResultsFile = experimentResultsDir ecf </> sign "client-results"
        , cLocalMemaslapConfigFile = experimentLocalTmpDir ecf </> sign "memaslap-config"
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
                , msConfigFile = experimentRemoteTmpDir ecf </> sign "memaslapcfg"
                }
            }
        }
