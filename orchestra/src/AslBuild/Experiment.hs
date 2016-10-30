{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiment
    ( module AslBuild.Experiment
    , module AslBuild.Experiment.Types
    ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
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

generateTargetFor :: ExperimentCfg -> Rules ()
generateTargetFor ecf@ExperimentCfg{..} = do
    let rFile = resultsFile ecf

    target ~> need [rFile]

    rFile %> \_ -> do
        need [provisionLocalhostRule, memcachedBin, memaslapBin, outputJarFile]
        (ExperimentSetup{..}, vmsNeeded) <- getSetup ecf

        -- Provision the vms with everything they need
        provisionVmsFromData vmsNeeded

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
        actionFinally (waitNicely $ toSeconds runtime) (return ())

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
        writeJSON rFile results

resultsFile :: ExperimentCfg -> FilePath
resultsFile ExperimentCfg{..} = resultsDir </> target ++ "-results" <.> jsonExt

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
    :: ExperimentCfg
    -> Action ([(RemoteLogin, String)], [(RemoteLogin, String)], [(RemoteLogin, String)], [VmData])
getVmsForExperiments ExperimentCfg{..} = case location of
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

getSetup :: ExperimentCfg -> Action (ExperimentSetup, [VmData])
getSetup ecf@ExperimentCfg{..} = do
    (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments ecf
    let serverPort = 12345
    let middlePort = 23456

    let experimentResultsDir = resultsDir </> target
    let experimentLocalTmpDir = tmpDir </> target
    let experimentRemoteTmpDir = "/tmp" </> target

    let servers = flip map (indexed sers) $ \(six, (sLogin, _)) -> ServerSetup
            { sRemoteLogin = sLogin
            , sIndex = six
            , sMemcachedFlags = MemcachedFlags
                { memcachedPort = serverPort + six
                , memcachedAsDaemon = True
                }
            }

    let (mLogin, mPrivate) = mid
    let middle = MiddleSetup
            { mRemoteLogin = mLogin
            , mLocalTrace = experimentResultsDir ++ "-trace" <.> csvExt
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
                , mwTraceFile = experimentRemoteTmpDir </> target ++ "-trace" <.> csvExt
                , mwVerbosity = logLevel
                }
            }

    let sign i f = target ++ "-" ++ f ++ "-" ++ show i
    let clients = flip map (indexed cls) $ \(cix, (cLogin, _)) -> ClientSetup
            { cRemoteLogin = cLogin
            , cIndex = cix
            , cLocalLog = experimentLocalTmpDir </> sign cix "client-local-log"
            , cRemoteLog = experimentRemoteTmpDir </> sign cix "memaslap-remote-log"
            , cResultsFile = experimentResultsDir </> sign cix "client-results"
            , cLocalMemaslapConfigFile = experimentLocalTmpDir </> sign cix "memaslap-config"
            , cMemaslapSettings = MemaslapSettings
                { msConfig = MemaslapConfig
                    { keysizeDistributions = [Distribution 16 16 1]
                    , valueDistributions = [Distribution 128 128 1]
                    , setProportion = 0.01
                    , getProportion = 0.99
                    }
                , msFlags = MemaslapFlags
                    { msServers = [RemoteServerUrl mPrivate middlePort]
                    , msThreads = 1
                    , msConcurrency = 64
                    , msOverwrite = 0.9
                    , msStatFreq = Just $ Seconds 1
                    , msWorkload = WorkFor runtime
                    , msConfigFile = experimentRemoteTmpDir </> sign cix "memaslapcfg"
                    }
                }
            }

    let setup = ExperimentSetup
            { esRuntime = runtime
            , clientSetups = clients
            , middleSetup = middle
            , serverSetups = servers
            }

    return (setup, vmsNeeded)
