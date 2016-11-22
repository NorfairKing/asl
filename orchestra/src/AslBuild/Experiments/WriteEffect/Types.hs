{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.WriteEffect.Types where

import           Data.Aeson
import           Data.List           (intercalate, nub)
import           GHC.Generics

import           AslBuild.Client
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Types
import           AslBuild.Utils

data WriteEffectCfg
    = WriteEffectCfg
    { hlConfig         :: HighLevelConfig
    , writePercentages :: [Double]
    , serverCounts     :: [Int]
    , weRuntime        :: TimeUnit
    } deriving (Show, Eq, Generic)

instance ToJSON   WriteEffectCfg
instance FromJSON WriteEffectCfg

instance ExperimentConfig WriteEffectCfg where
    highLevelConfig = hlConfig
    genExperimentSetups stc@WriteEffectCfg{..} = do
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments stc True

        let setups = do
                writePercentage <- writePercentages
                curNrServers <- serverCounts
                replicationFactor <- nub [1, curNrServers]
                let signGlobally f = intercalate "-" [f, show curNrServers, show replicationFactor, flatPercent writePercentage]
                let servers = take curNrServers $ genServerSetups sers

                let rSampleRate = 1000
                let wSampleRate = floor $ writePercentage * fromIntegral rSampleRate

                let defaultMiddle = genMiddleSetup stc mid servers sers signGlobally
                let middle = defaultMiddle
                        { mMiddlewareFlags = (mMiddlewareFlags defaultMiddle)
                            { mwReplicationFactor = replicationFactor
                            , mwReadSampleRate = Just rSampleRate
                            , mwWriteSampleRate = Just wSampleRate
                            }
                        }

                let defaultClients = genClientSetup stc cls (middleRemoteServer middle) signGlobally weRuntime
                let clients = flip map defaultClients $ \cs -> cs
                        { cMemaslapSettings = (cMemaslapSettings cs)
                            { msConfig = (msConfig $ cMemaslapSettings cs)
                                { setProportion = writePercentage
                                }
                            }
                        }

                return $ genExperimentSetup stc weRuntime clients middle servers signGlobally
        return (setups, vmsNeeded)
