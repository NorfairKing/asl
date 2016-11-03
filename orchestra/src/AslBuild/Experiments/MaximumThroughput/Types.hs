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
    { hlConfig          :: HighLevelConfig
    , clientCountTuples :: [(Int, Int)]
    } deriving (Show, Eq, Generic)

instance ToJSON   MaximumThroughputCfg
instance FromJSON MaximumThroughputCfg

instance ExperimentConfig MaximumThroughputCfg where
    highLevelConfig = hlConfig
    genExperimentSetups stc@MaximumThroughputCfg{..} = do
        let runtime = Seconds 5
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments stc

        let setups = do
                (curNrClients, curConcurrency) <- clientCountTuples
                let signGlobally f = intercalate "-" [f, show curNrClients, show curConcurrency]
                let servers = genServerSetups sers

                let defaultMiddle = genMiddleSetup stc mid servers sers signGlobally
                let middle = defaultMiddle
                        { mMiddlewareFlags = (mMiddlewareFlags defaultMiddle)
                            { mwReplicationFactor = 1
                            }
                        }

                let defaultClients = genClientSetup stc cls middle signGlobally runtime
                let clients = take curNrClients $ flip map defaultClients $ \cs ->
                        let sets = cMemaslapSettings cs
                            config = msConfig sets
                            flags = msFlags sets
                        in cs
                        { cMemaslapSettings = sets
                            { msConfig = config
                                { setProportion = 0 -- There will be some sets anyway, in the warmup phase, and we have to handle that.
                                }
                            , msFlags = flags
                                { msConcurrency = curConcurrency
                                }
                            }
                        }

                return $ genExperimentSetup stc runtime clients middle servers signGlobally
        return (setups, vmsNeeded)
