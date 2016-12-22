{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module AslBuild.Experiments.Extreme.Types where

import Data.Aeson
import GHC.Generics

import AslBuild.Client
import AslBuild.Experiment
import AslBuild.Memaslap
import AslBuild.Middle
import AslBuild.Middleware
import AslBuild.Types

data ExtremeCfg = ExtremeCfg
    { hlConfig :: HighLevelConfig
    , exRuntime :: TimeUnit
    } deriving (Show, Eq, Generic)

instance FromJSON ExtremeCfg

instance ToJSON ExtremeCfg

instance ExperimentConfig ExtremeCfg where
    highLevelConfig = hlConfig
    genExperimentSetups fc@ExtremeCfg {..} = do
        let HighLevelConfig {..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments fc True
        let setups = do
                let signGlobally = id
                let servers = genServerSetups sers
                let defaultMiddle = genMiddleSetup fc mid servers sers signGlobally
                let middle =
                        modif defaultMiddle $ \m ->
                            m
                            { mMiddlewareFlags =
                                  (mMiddlewareFlags m)
                                  {mwReplicationFactor = (length sers `div` 2) + 1}
                            }
                let defaultClients =
                        genClientSetup fc cls (middleRemoteServer . middle) signGlobally exRuntime
                let clients =
                        modif defaultClients $ \defcs ->
                            flip map defcs $ \cs ->
                                let sets = cMemaslapSettings cs
                                    config = msConfig sets
                                in cs
                                   { cMemaslapSettings =
                                         sets {msConfig = config {setProportion = 0.1}}
                                   }
                pure $ genExperimentSetup fc exRuntime clients middle servers signGlobally
        pure (setups, vmsNeeded)
