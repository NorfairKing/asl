{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.ReplicationEffect.Types where

import           Data.Aeson
import           Data.List           (intercalate)
import           GHC.Generics

import           AslBuild.Client
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Types

data ReplicationEffectCfg
    = ReplicationEffectCfg
    { hlConfig           :: HighLevelConfig
    , serverCounts       :: [Int]
    , replicationFactors :: [Double]
    , reRuntime          :: TimeUnit
    } deriving (Show, Eq, Generic)

instance ToJSON   ReplicationEffectCfg
instance FromJSON ReplicationEffectCfg

instance ExperimentConfig ReplicationEffectCfg where
    highLevelConfig = hlConfig
    genExperimentSetups stc@ReplicationEffectCfg{..} = do
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments stc True

        let setups = do
                curNrServers <- serverCounts
                replicationFactor <- replicationFactors
                let signGlobally f = intercalate "-" [f, show replicationFactor, show curNrServers]

                let servers = take curNrServers $ genServerSetups sers

                let setProp = 0.05
                let rSampleRate = 1000
                let wSampleRate = floor $ setProp * fromIntegral rSampleRate

                let defaultMiddle = genMiddleSetup stc mid servers sers signGlobally
                let middle = defaultMiddle
                        { mMiddlewareFlags = (mMiddlewareFlags defaultMiddle)
                            { mwReplicationFactor = max 1 $ ceiling $ fromIntegral (length servers) * replicationFactor
                            , mwReadSampleRate = Just rSampleRate
                            , mwWriteSampleRate = Just wSampleRate
                            }
                        }

                let defaultClients = genClientSetup stc cls (middleRemoteServer middle) signGlobally reRuntime
                let clients = flip map defaultClients $ \cs -> cs
                        { cMemaslapSettings = (cMemaslapSettings cs)
                            { msConfig = (msConfig $ cMemaslapSettings cs)
                                { setProportion = setProp
                                }
                            }
                        }

                return $ genExperimentSetup stc reRuntime clients middle servers signGlobally
        return (setups, vmsNeeded)
