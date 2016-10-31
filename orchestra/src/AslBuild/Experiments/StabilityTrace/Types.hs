{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.StabilityTrace.Types where

import           Data.Aeson
import           GHC.Generics

import           Development.Shake.FilePath

import           AslBuild.Client
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Server
import           AslBuild.Types
import           AslBuild.Utils

data StabilityTraceCfg
    = StabilityTraceCfg
    { hlConfig :: HighLevelConfig
    , runtime  :: TimeUnit
    , logLevel :: LogLevel
    } deriving (Show, Eq, Generic)

instance ToJSON   StabilityTraceCfg
instance FromJSON StabilityTraceCfg

instance ExperimentConfig StabilityTraceCfg where
    highLevelConfig = hlConfig
    genExperimentSetups stc@StabilityTraceCfg{..} = do
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments stc
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
                , mLocalTrace = experimentResultsDir </> target ++ "-trace" <.> csvExt
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
                , esResultsSummaryFile = experimentResultsDir </> "summary.json"
                , clientSetups = clients
                , middleSetup = middle
                , serverSetups = servers
                }

        return ([setup], vmsNeeded)


