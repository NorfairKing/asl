{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.MaximumThroughput.Types where

import           Data.Aeson
import           Data.List           (intercalate)
import           GHC.Generics

import           AslBuild.Client
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Types

data MaximumThroughputCfg
    = MaximumThroughputCfg
    { hlConfig       :: HighLevelConfig
    , threadConcTups :: [(Int, Int)]
    , mtRuntime      :: TimeUnit
    } deriving (Show, Eq, Generic)

instance ToJSON   MaximumThroughputCfg
instance FromJSON MaximumThroughputCfg

instance ExperimentConfig MaximumThroughputCfg where
    highLevelConfig = hlConfig
    genExperimentSetups stc@MaximumThroughputCfg{..} = do
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments stc True

        let setups = do
                (curMiddleThreads, curConcurrency) <- threadConcTups
                let signGlobally f = intercalate "-" [f, show curMiddleThreads, show curConcurrency]
                let servers = genServerSetups sers

                let defaultMiddle = genMiddleSetup stc mid servers sers signGlobally
                let middle = defaultMiddle
                        { mMiddlewareFlags = (mMiddlewareFlags defaultMiddle)
                            { mwReplicationFactor = 1
                            , mwNrThreads = curMiddleThreads
                            , mwWriteSampleRate = Just 0
                            , mwReadSampleRate = Just 100
                            }
                        }

                let defaultClients = genClientSetup stc cls (middleRemoteServer middle) signGlobally mtRuntime
                let clients = flip map defaultClients $ \cs ->
                        let sets = cMemaslapSettings cs
                            config = msConfig sets
                            flags = msFlags sets
                        in cs
                        { cMemaslapSettings = sets
                            { msConfig = config
                                { setProportion = 0
                                }
                            , msFlags = flags
                                { msConcurrency = curConcurrency
                                }
                            }
                        }

                return $ genExperimentSetup stc mtRuntime clients middle servers signGlobally
        return (setups, vmsNeeded)
