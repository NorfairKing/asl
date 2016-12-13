{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.ThinkTime.Types where

import           Data.Aeson
import           GHC.Generics

import           AslBuild.Client
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Types

data ThinkTimeCfg
    = ThinkTimeCfg
    { hlConfig  :: HighLevelConfig
    , ttRuntime :: TimeUnit
    } deriving (Show, Eq, Generic)

instance ToJSON ThinkTimeCfg
instance FromJSON ThinkTimeCfg

instance ExperimentConfig ThinkTimeCfg where
    highLevelConfig = hlConfig
    genExperimentSetups ttc@ThinkTimeCfg{..} = do
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments ttc True
        let signGlobally = id
        let servers = genServerSetups sers
        let defaultMiddle = genMiddleSetup ttc mid servers sers signGlobally
        let middle = modif defaultMiddle $ \m -> m
                { mMiddlewareFlags = (mMiddlewareFlags m)
                    { mwReadSampleRate = Just 1
                    , mwWriteSampleRate = Just 1
                    , mwNrThreads = 1
                    }
                }
        let defaultClients = genClientSetup ttc cls (middleRemoteServer . middle) signGlobally ttRuntime
        let clients = modif defaultClients $ \defcs -> flip map defcs $ \cs ->
                let sets = cMemaslapSettings cs
                    config = msConfig sets
                    flags = msFlags sets
                in cs
                { cMemaslapSettings = sets
                    { msConfig = config
                        { setProportion = 0.01
                        }
                    , msFlags = flags
                        { msConcurrency = 1
                        }
                    }
                }
        let setup = genExperimentSetup ttc ttRuntime clients middle servers signGlobally
        pure ([setup], vmsNeeded)
