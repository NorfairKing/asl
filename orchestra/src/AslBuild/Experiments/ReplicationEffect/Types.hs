{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.ReplicationEffect.Types where

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

data ReplicationEffectCfg
    = ReplicationEffectCfg
    { hlConfig           :: HighLevelConfig
    , serverCounts       :: [Int]
    , replicationFactors :: [Double]
    } deriving (Show, Eq, Generic)

instance ToJSON   ReplicationEffectCfg
instance FromJSON ReplicationEffectCfg

instance ExperimentConfig ReplicationEffectCfg where
    highLevelConfig = hlConfig
    genExperimentSetups stc@ReplicationEffectCfg{..} = do
        let runtime = Seconds 5
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments stc
        let middlePort = 23456

        let setups = do
                curNrServers <- serverCounts
                replicationFactor <- replicationFactors
                let signGlobally f = intercalate "-" [f, show curNrServers, show replicationFactor]

                let servers = take curNrServers $ genServerSetups sers

                let (mLogin, mPrivate) = mid
                let traceFileName = signGlobally (target ++ "-trace")
                let middle = MiddleSetup
                        { mRemoteLogin = mLogin
                        , mLocalTrace = experimentResultsDir stc </> traceFileName <.> csvExt
                        , mMiddlewareFlags = MiddlewareFlags
                            { mwIp = mPrivate
                            , mwPort = middlePort
                            , mwNrThreads = 1
                            , mwReplicationFactor = max 1 $ floor $ fromIntegral (length servers) * replicationFactor
                            , mwServers = map
                                (\(ServerSetup{..}, (_, sPrivate)) ->
                                    RemoteServerUrl
                                        sPrivate
                                        (memcachedPort sMemcachedFlags))
                                (zip servers sers)
                            , mwTraceFile = experimentRemoteTmpDir stc </> traceFileName <.> csvExt
                            , mwVerbosity = LogOff
                            }
                        }
                let clients = flip map (indexed cls) $ \(cix, (cLogin, _)) ->
                        let sign f = signGlobally $ intercalate "-" [target, show cix, f]
                        in ClientSetup
                            { cRemoteLogin = cLogin
                            , cIndex = cix
                            , cLocalLog = experimentLocalTmpDir stc </> sign "client-local-log"
                            , cRemoteLog = experimentRemoteTmpDir stc </> sign "memaslap-remote-log"
                            , cResultsFile = experimentResultsDir stc </> sign "client-results"
                            , cLocalMemaslapConfigFile = experimentLocalTmpDir stc </> sign "memaslap-config"
                            , cMemaslapSettings = MemaslapSettings
                                { msConfig = defaultMemaslapConfig
                                    { setProportion = 0.05
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

                let summaryFileName = signGlobally "summary"
                return ExperimentSetup
                    { esRuntime = runtime
                    , esResultsSummaryFile = experimentResultsDir stc </> summaryFileName <.> jsonExt
                    , clientSetups = clients
                    , middleSetup = middle
                    , serverSetups = servers
                    }

        return (setups, vmsNeeded)



