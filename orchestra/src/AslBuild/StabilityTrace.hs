{-# LANGUAGE RecordWildCards #-}
module AslBuild.StabilityTrace
    ( module AslBuild.StabilityTrace
    , module AslBuild.StabilityTrace.Types
    ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import qualified Data.Aeson.Encode.Pretty      as A
import qualified Data.ByteString.Lazy          as LB
import           System.Directory
import           System.Process

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.BuildMemcached
import           AslBuild.Client
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Provision
import           AslBuild.Server
import           AslBuild.StabilityTrace.Types
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm

stabilityTraceRules :: Rules ()
stabilityTraceRules = do
    generateTargetFor smallLocalStabilityTrace
    generateTargetFor localStabilityTrace
    generateTargetFor bigLocalStabilityTrace
    generateTargetFor smallRemoteStabilityTrace
    generateTargetFor remoteStabilityTrace

smallLocalStabilityTraceRule :: String
smallLocalStabilityTraceRule = "small-local-stability-trace"

smallLocalStabilityTrace :: StabilityTraceCfg
smallLocalStabilityTrace = StabilityTraceCfg
    { target = smallLocalStabilityTraceRule
    , csvOutFile = tmpDir
        </> smallLocalStabilityTraceRule </> "results"
        </> "small-local-stability-trace-results.csv"
    , nrServers = 1
    , nrClients = 1
    , location = StabilityLocal
    , runtime = Seconds 5
    , logLevel = LogFiner
    }

localStabilityTracelRule :: String
localStabilityTracelRule = "local-stability-trace"

localStabilityTrace :: StabilityTraceCfg
localStabilityTrace = StabilityTraceCfg
    { target = localStabilityTracelRule
    , csvOutFile = resultsDir </> "local-stability-trace-results.csv"
    , nrServers = nrServers remoteStabilityTrace
    , nrClients = nrClients remoteStabilityTrace
    , location = StabilityLocal
    , runtime = runtime remoteStabilityTrace
    , logLevel = LogOff
    }

bigLocalStabilityTraceRule :: String
bigLocalStabilityTraceRule = "big-local-stability-trace"

bigLocalStabilityTrace :: StabilityTraceCfg
bigLocalStabilityTrace = StabilityTraceCfg
    { target = bigLocalStabilityTraceRule
    , csvOutFile = resultsDir </> "big-local-stability-trace-results.csv"
    , nrServers = 8
    , nrClients = 32
    , location = StabilityLocal
    , runtime = Hours 1
    , logLevel = LogOff
    }

smallRemoteStabilityTraceRule :: String
smallRemoteStabilityTraceRule = "small-remote-stability-trace"

smallRemoteStabilityTrace :: StabilityTraceCfg
smallRemoteStabilityTrace = StabilityTraceCfg
    { target = smallRemoteStabilityTraceRule
    , csvOutFile = resultsDir </> "small-remote-stability-trace-results.csv"
    , nrServers = 3
    , nrClients = 3
    , location = StabilityRemote
    , runtime = Seconds 10
    , logLevel = LogOff
    }


remoteStabilityTraceRule :: String
remoteStabilityTraceRule = "remote-stability-trace"

remoteStabilityTrace :: StabilityTraceCfg
remoteStabilityTrace = StabilityTraceCfg
    { target = remoteStabilityTraceRule
    , csvOutFile = resultsDir </> "remote-stability-trace-results.csv"
    , nrServers = 3
    , nrClients = 3
    , location = StabilityRemote
    , runtime = Hours 1
    , logLevel = LogOff
    }


generateTargetFor :: StabilityTraceCfg -> Rules ()
generateTargetFor stc@StabilityTraceCfg{..} = do
    target ~> need [csvOutFile]

    csvOutFile %> \_ -> do
        need [memcachedBin, memaslapBin, outputJarFile]
        (StabilityTraceSetup{..}, vmsNeeded) <- getSetup stc

        need [provisionLocalhostRule]
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
        actionFinally (waitNicely $ toSeconds stsRuntime) (return ())

        -- Shut down the middleware
        shutdownMiddle middleSetup
        void $ liftIO $ waitForProcess middlePh

        -- Shut down the servers
        shutdownServers serverSetups

        -- Copy the middleware logs back
        copyMiddleTraceBack middleSetup

        -- Wait for memaslap to finish writing logs.
        wait 1

        -- Copy the client logs back
        copyClientLogsBack clientSetups

        -- Prepare analysis files for the client logs.
        forP_ clientSetups $ \cSetup@ClientSetup{..} -> do
            mel <- parseLog cLocalLog
            case mel of
                Nothing -> fail $ "could not parse logfile: " ++ cLocalLog
                Just parsedLog -> do
                    let results = StabilityTraceExperimentResults
                            { sterClientSetup = cSetup
                            , sterMemaslapLog = parsedLog
                            , sterClientIndex = cIndex
                            }
                    liftIO $ do
                        createDirectoryIfMissing True $ takeDirectory cResultsFile
                        LB.writeFile cResultsFile $ A.encodePretty results

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

getSetup :: StabilityTraceCfg -> Action (StabilityTraceSetup, [VmData])
getSetup StabilityTraceCfg{..} = do
    (cls, [mid], sers, vmsNeeded) <- case location of
        StabilityLocal -> do
            let localLogin = RemoteLogin Nothing localhostIp
            let localPrivate = localhostIp
            let localTup = (localLogin, localPrivate)
            let locals = repeat localTup
            return (take nrClients locals, take 1 locals, take nrServers locals, [])
        StabilityRemote -> do
            (cs, ms, ss) <- getVms nrClients 1 nrServers
            let login VmData{..} = RemoteLogin (Just vmAdmin) vmFullUrl
            let private VmData{..} = vmPrivateIp
            let tups = map (login &&& private)
            return (tups cs, tups ms, tups ss, cs ++ ms ++ ss)

    let serverPort = 12345
    let middlePort = 23456

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
            , mLocalTrace = csvOutFile
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
                , mwTraceFile = "/tmp" </> target ++ "-trace" <.> csvExt
                , mwVerbosity = logLevel
                }
            }

    let experimentTmpDir = tmpDir </> target

    let sign i f = target ++ "-" ++ f ++ "-" ++ show i
    let clients = flip map (indexed cls) $ \(cix, (cLogin, _)) -> ClientSetup
            { cRemoteLogin = cLogin
            , cIndex = cix
            , cLocalLog = experimentTmpDir </> sign cix "client-local-log"
            , cRemoteLog = "/tmp" </> sign cix "memaslap-log"
            , cResultsFile = resultsDir </> target </> sign cix "stabilitytmpresults"
            , cLocalMemaslapConfigFile = experimentTmpDir </> sign cix "distribution"
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
                    , msConfigFile = "/tmp" </> sign cix "memaslapcfg"
                    }
                }
            }

    let setup = StabilityTraceSetup
            { stsRuntime = runtime
            , clientSetups = clients
            , middleSetup = middle
            , serverSetups = servers
            }

    return (setup, vmsNeeded)
