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

        let servers = genServerSetups sers

        let (mLogin, mPrivate) = mid
        let defaultMiddle = genMiddleSetup stc mid servers sers
        let middle = defaultMiddle
                { mMiddlewareFlags = (mMiddlewareFlags defaultMiddle)
                    { mwReplicationFactor = length servers
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
                            { msServers = [middleRemoteServer middle]
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


