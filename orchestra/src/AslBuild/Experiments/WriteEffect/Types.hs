{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiments.WriteEffect.Types where

import           Data.Aeson
import           Data.List                  (intercalate, nub)
import           GHC.Generics

import           Development.Shake.FilePath

import           AslBuild.Client
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Types

data WriteEffectCfg
    = WriteEffectCfg
    { hlConfig         :: HighLevelConfig
    , writePercentages :: [Double]
    , serverCounts     :: [Int]
    } deriving (Show, Eq, Generic)

instance ToJSON   WriteEffectCfg
instance FromJSON WriteEffectCfg

instance ExperimentConfig WriteEffectCfg where
    highLevelConfig = hlConfig
    genExperimentSetups stc@WriteEffectCfg{..} = do
        let runtime = Seconds 5
        let HighLevelConfig{..} = hlConfig
        (cls, [mid], sers, vmsNeeded) <- getVmsForExperiments stc

        let setups = do
                curNrServers <- serverCounts
                replicationFactor <- nub [1, curNrServers]
                writePercentage <- writePercentages
                let signGlobally f = intercalate "-" [f, show curNrServers, show replicationFactor, show writePercentage]
                let servers = take curNrServers $ genServerSetups sers

                let defaultMiddle = genMiddleSetup stc mid servers sers signGlobally
                let middle = defaultMiddle
                        { mMiddlewareFlags = (mMiddlewareFlags defaultMiddle)
                            { mwReplicationFactor = replicationFactor
                            }
                        }

                let defaultClients = genClientSetup stc cls middle signGlobally runtime
                let clients = flip map defaultClients $ \cs -> cs
                        { cMemaslapSettings = (cMemaslapSettings cs)
                            { msConfig = defaultMemaslapConfig
                                { setProportion = writePercentage
                                }
                            }
                        }

                let summaryFileName = signGlobally "summary"
                return ExperimentSetup
                    { esRuntime = runtime
                    , esResultsSummaryFile = experimentResultsDir stc </> summaryFileName <.> jsonExt
                    , clientSetups = clients
                    , middleSetup = middle
                    , serverSetups = servers
                    }

        return (setups, vmsNeeded)



