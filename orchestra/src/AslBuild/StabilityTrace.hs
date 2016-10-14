{-# LANGUAGE RecordWildCards #-}
module AslBuild.StabilityTrace
    ( module AslBuild.StabilityTrace
    , module AslBuild.StabilityTrace.Types
    ) where

import           Control.Monad
import           Data.List
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

smallLocalStabilityTraceRule :: String
smallLocalStabilityTraceRule = "small-local-stability-trace"

smallLocalStabilityTrace :: StabilityTraceCfg
smallLocalStabilityTrace = StabilityTraceCfg
    { target = smallLocalStabilityTraceRule
    , csvOutFile = resultsDir </> "small-local-stability-trace-results.csv"
    , nrServers = 1
    , nrClients = 1
    , location = StabilityLocal
    , runtime = Seconds 5
    }

localStabilityTracelRule :: String
localStabilityTracelRule = "local-stability-trace"

localStabilityTrace :: StabilityTraceCfg
localStabilityTrace = StabilityTraceCfg
    { target = localStabilityTracelRule
    , csvOutFile = resultsDir </> "local-stability-trace-results.csv"
    , nrServers = 3
    , nrClients = 3
    , location = StabilityLocal
    , runtime = Hours 1
    }

remoteStabilityTracelRule :: String
remoteStabilityTracelRule = "remote-stability-trace"

remoteStabilityTrace :: StabilityTraceCfg
remoteStabilityTrace = StabilityTraceCfg
    { target = remoteStabilityTracelRule
    , csvOutFile = resultsDir </> "remote-stability-trace-results.csv"
    , nrServers = 3
    , nrClients = 3
    , location = StabilityRemote
    , runtime = Hours 1
    }


generateTargetFor :: StabilityTraceCfg -> Rules ()
generateTargetFor stc@StabilityTraceCfg{..} = do
    target ~> need [csvOutFile]

    csvOutFile %> \_ -> do
        need [memcachedBin, memaslapBin, outputJarFile]
        StabilityTraceSetup{..} <- getSetup stc

        case location of
            StabilityLocal -> need [provisionLocalhostRule]
            StabilityRemote -> do
                -- TODO, only start the Vms we use.
                need [startVmsRule]
                provisionVms $ nub $ map cRemoteLogin clientSetups
                provisionVms [mRemoteLogin middleSetup]
                provisionVms $ nub $ map sRemoteLogin serverSetups


        -- Get the clients configs set up
        setupClientConfigs clientSetups

        -- Start the servers
        startServersOn serverSetups

        -- Wait for the servers to get started
        waitMs 250

        -- Start the middleware
        middlePh <- startMiddleOn middleSetup

        -- Start the clients
        startClientsOn clientSetups

        -- Wait long enough to be sure that all clients are done
        wait $ toSeconds stsRuntime

        -- Shut down the middleware
        shutdownMiddle middleSetup
        void $ liftIO $ waitForProcess middlePh

        -- Shut down the servers
        shutdownServers serverSetups

        -- Copy the middleware logs back
        copyMiddleTraceBack middleSetup

getSetup :: StabilityTraceCfg -> Action StabilityTraceSetup
getSetup StabilityTraceCfg{..} = do
    (cs, [m], ss) <- case location of
        StabilityLocal -> do
            let local = RemoteLogin Nothing localhostIp
            let locals = repeat local
            return (take nrClients locals, take 1 locals, take nrServers locals)
        _ -> fail "failed to get vm setups"

    let serverPort = 12345
    let middlePort = 23456

    let servers = flip map (indexed ss) $ \(six, srl) -> ServerSetup
            { sRemoteLogin = srl
            , sIndex = six
            , sMemcachedFlags = MemcachedFlags
                { memcachedPort = serverPort + six
                , memcachedAsDaemon = True
                }
            }

    let middle = MiddleSetup
            { mRemoteLogin = m
            , mLocalTrace = csvOutFile
            , mMiddlewareFlags = MiddlewareFlags
                -- TODO private Ip's of middleware
                { mwIp = localhostIp
                , mwPort = middlePort
                , mwNrThreads = 1
                , mwReplicationFactor = length servers
                -- TODO private Ip's of servers
                , mwServers = map
                    (\ServerSetup{..} ->
                        RemoteServerUrl
                            (remoteHost sRemoteLogin)
                            (memcachedPort sMemcachedFlags))
                    servers
                , mwTraceFile = "/tmp" </> target ++ "-trace" <.> csvExt
                , mwVerbosity = LogOff
                }
            }

    let experimentTmpDir = tmpDir </> target

    let sign i f = target ++ "-" ++ f ++ "-" ++ show i
    let clients = flip map (indexed cs) $ \(cix, crl) -> ClientSetup
            { cRemoteLogin = crl
            , cIndex = cix
            , cLocalLog = experimentTmpDir </> sign cix "client-local-log"
            , cRemoteLog = "/tmp" </> sign cix "memaslap-log"
            , cResultsFile = experimentTmpDir </> sign cix "stabilitytmpresults"
            , cLocalMemaslapConfigFile = experimentTmpDir </> sign cix "distribution"
            , cMemaslapSettings = MemaslapSettings
                { msConfig = MemaslapConfig
                    { keysizeDistributions = [Distribution 16 16 1]
                    , valueDistributions = [Distribution 128 128 1]
                    , setProportion = 0.01
                    , getProportion = 0.99
                    }
                , msFlags = MemaslapFlags
                    -- TODO private IP of middleware
                    { msServers = [RemoteServerUrl localhostIp middlePort]
                    , msThreads = 1
                    , msConcurrency = 1
                    , msOverwrite = 0.9
                    , msStatFreq = Just runtime
                    , msWorkload = WorkFor runtime
                    , msConfigFile = "/tmp" </> sign cix "memaslapcfg"
                    }
                }
            }

    return StabilityTraceSetup
        { stsRuntime = runtime
        , clientSetups = clients
        , middleSetup = middle
        , serverSetups = servers
        }








