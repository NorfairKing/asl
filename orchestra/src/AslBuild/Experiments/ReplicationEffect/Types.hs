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
    } deriving (Show, Eq, Generic)

instance ToJSON   ReplicationEffectCfg
instance FromJSON ReplicationEffectCfg

instance ExperimentConfig ReplicationEffectCfg where
    highLevelConfig = hlConfig
    genExperimentSetups stc@ReplicationEffectCfg{..} = do
        let runtime = Seconds 5
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments stc

        let setups = do
                curNrServers <- serverCounts
                replicationFactor <- replicationFactors
                let signGlobally f = intercalate "-" [f, show curNrServers, show replicationFactor]

                let servers = take curNrServers $ genServerSetups sers

                let defaultMiddle = genMiddleSetup stc mid servers sers signGlobally
                let middle = defaultMiddle
                        { mMiddlewareFlags = (mMiddlewareFlags defaultMiddle)
                            { mwReplicationFactor = max 1 $ ceiling $ fromIntegral (length servers) * replicationFactor
                            }
                        }

                let defaultClients = genClientSetup stc cls middle signGlobally runtime
                let clients = flip map defaultClients $ \cs -> cs
                        { cMemaslapSettings = (cMemaslapSettings cs)
                            { msConfig = defaultMemaslapConfig
                                { setProportion = 0.05
                                }
                            }
                        }

                return $ genExperimentSetup stc runtime clients middle servers signGlobally
        return (setups, vmsNeeded)
