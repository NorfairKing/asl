{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.StabilityTrace.Types where

import           Data.Aeson
import           GHC.Generics

import           Development.Shake.FilePath

import           AslBuild.Client
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Types

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

        let signGlobally = id
        let defaultMiddle = genMiddleSetup stc mid servers sers signGlobally
        let middle = defaultMiddle
                { mMiddlewareFlags = (mMiddlewareFlags defaultMiddle)
                    { mwReplicationFactor = length servers
                    , mwVerbosity = logLevel
                    }
                }

        let defaultClients = genClientSetup stc cls middle signGlobally runtime
        let clients = flip map defaultClients $ \cs -> cs
                { cMemaslapSettings = (cMemaslapSettings cs)
                    { msConfig = defaultMemaslapConfig
                        { setProportion = 0.01
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


