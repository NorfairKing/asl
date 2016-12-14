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
    { hlConfig       :: HighLevelConfig
    , fRuntime       :: TimeUnit
    , virtualClients :: (Int, Int)
    , keySizes       :: (Int, Int)
    , valueSizes     :: (Int, Int)
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
                curVirtualClients <- tupToList virtualClients
                curKeySize <- tupToList keySizes
                curValueSize <- tupToList valueSizes
                let signGlobally f = intercalate "-" [f, show curVirtualClients, show curKeySize, show curValueSize]

                let servers = genServerSetups sers
                let defaultMiddle = genMiddleSetup fc mid servers sers signGlobally
                let middle = modif defaultMiddle $ \m -> m
                        { mMiddlewareFlags = (mMiddlewareFlags m)
                            { mwNrThreads = 1
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
                                { keysizeDistributions = [Distribution curKeySize curKeySize 1]
                                , valueDistributions = [Distribution curValueSize curValueSize 1]
                                }
                            , msFlags = flags
                                { msConcurrency = curVirtualClients
                                }
                            }
                        }
                pure $ genExperimentSetup fc fRuntime clients middle servers signGlobally
        pure (setups, vmsNeeded)
