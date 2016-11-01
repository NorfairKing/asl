{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.StabilityTrace.Types where

import           Data.Aeson
import           GHC.Generics

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

        let setup = genExperimentSetup stc runtime clients middle servers signGlobally
        return ([setup], vmsNeeded)

