{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module AslBuild.Experiments.Factorial.Types where

import           Data.Aeson
import           Data.List
import           GHC.Generics

import           AslBuild.Client
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Types

data FactorialCfg
    = FactorialCfg
    { hlConfig                :: HighLevelConfig
    , fRuntime                :: TimeUnit
    , writePercentages        :: (Double, Double)
    , valueSizes              :: (Int, Int)
    , replicationCoefficients :: (Double, Double)
    } deriving (Show, Eq, Generic)

instance FromJSON FactorialCfg
instance ToJSON   FactorialCfg

instance ExperimentConfig FactorialCfg where
    highLevelConfig = hlConfig
    genExperimentSetups fc@FactorialCfg{..} = do
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments fc True
        let setups = do
                let tupToList (a, b) = [a, b]
                curWritePercentage <- tupToList writePercentages
                curValueSize <- tupToList valueSizes
                curReplicationCoefficient <- tupToList replicationCoefficients
                let signGlobally f = intercalate "-" [f, show curWritePercentage, show curValueSize, show curReplicationCoefficient]

                let servers = genServerSetups sers
                let defaultMiddle = genMiddleSetup fc mid servers sers signGlobally
                let middle = modif defaultMiddle $ \m -> m
                        { mMiddlewareFlags = (mMiddlewareFlags m)
                            {   mwReplicationFactor =
                                if curReplicationCoefficient == 0
                                then 1
                                else length sers
                            }
                        }
                let defaultClients = genClientSetup fc cls (middleRemoteServer . middle) signGlobally fRuntime
                let clients = modif defaultClients $ \defcs -> flip map defcs $ \cs ->
                        let sets = cMemaslapSettings cs
                            config = msConfig sets
                            flags = msFlags sets
                        in cs
                        { cMemaslapSettings = sets
                            { msConfig = config
                                { valueDistributions = [Distribution curValueSize curValueSize 1]
                                , setProportion = curWritePercentage
                                }
                            }
                        }
                pure $ genExperimentSetup fc fRuntime clients middle servers signGlobally
        pure (setups, vmsNeeded)
