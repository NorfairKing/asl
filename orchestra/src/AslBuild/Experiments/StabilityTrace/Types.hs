{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.StabilityTrace.Types where

import           Data.Aeson
import           Data.List                  (intercalate)
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
        let middlePort = 23456

        let servers = genServerSetups sers

        let (mLogin, mPrivate) = mid
        let middle = MiddleSetup
                { mRemoteLogin = mLogin
                , mLocalTrace = experimentResultsDir stc </> target ++ "-trace" <.> csvExt
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
                    , mwTraceFile = experimentRemoteTmpDir stc </> target ++ "-trace" <.> csvExt
                    , mwVerbosity = logLevel
                    }
                }
        let clients = flip map (indexed cls) $ \(cix, (cLogin, _)) ->
                let sign f = intercalate "-" [target, f, show cix]
                in ClientSetup
                    { cRemoteLogin = cLogin
                    , cIndex = cix
                    , cLocalLog = experimentLocalTmpDir stc </> sign "client-local-log"
                    , cRemoteLog = experimentRemoteTmpDir stc </> sign "memaslap-remote-log"
                    , cResultsFile = experimentResultsDir stc </> sign "client-results"
                    , cLocalMemaslapConfigFile = experimentLocalTmpDir stc </> sign "memaslap-config"
                    , cMemaslapSettings = MemaslapSettings
                        { msConfig = defaultMemaslapConfig
                            { setProportion = 0.01
                            }
                        , msFlags = MemaslapFlags
                            { msServers = [RemoteServerUrl mPrivate middlePort]
                            , msThreads = 1
                            , msConcurrency = 64
                            , msOverwrite = 0.9
                            , msStatFreq = Just $ Seconds 1
                            , msWorkload = WorkFor runtime
                            , msConfigFile = experimentRemoteTmpDir stc </> sign "memaslapcfg"
                            }
                        }
                    }

        let setup = ExperimentSetup
                { esRuntime = runtime
                , esResultsSummaryFile = experimentResultsDir stc </> "summary.json"
                , clientSetups = clients
                , middleSetup = middle
                , serverSetups = servers
                }

        return ([setup], vmsNeeded)


