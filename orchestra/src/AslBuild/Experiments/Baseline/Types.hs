{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Experiments.Baseline.Types where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.List
import           GHC.Generics

import           Development.Shake.FilePath

import           AslBuild.Client.Types
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Server
import           AslBuild.Types


data BaselineExperimentRuleCfg
    = BaselineExperimentRuleCfg
    { hlConfig      :: HighLevelConfig
    , concurrencies :: [Int]
    , blRuntime     :: TimeUnit
    , repetitions   :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON   BaselineExperimentRuleCfg
instance FromJSON BaselineExperimentRuleCfg

instance ExperimentConfig BaselineExperimentRuleCfg where
    highLevelConfig = hlConfig
    genExperimentSetups ecf@BaselineExperimentRuleCfg{..} = do
        let HighLevelConfig{..} = hlConfig
        (cls, [], sers@[(_, serPrivateIp)], vmsNeeded) <- getVmsForExperiments ecf False
        let setups = do
                curConcurrency <- concurrencies
                let signGlobally f = intercalate "-" [f, show curConcurrency]

                let [server] = genServerSetups sers
                let defaultClients = genClientSetup
                        ecf
                        cls
                        (RemoteServerUrl serPrivateIp $ memcachedPort $ sMemcachedFlags server)
                        signGlobally
                        blRuntime
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
                return $ genBaselineExperimentSetup ecf blRuntime clients server signGlobally
        return (setups, vmsNeeded)

-- TODO Unify with the same thing in 'AslBuild.Experiment'
genBaselineExperimentSetup
    :: ExperimentConfig a
    => a
    -> TimeUnit
    -> [ClientSetup]
    -> ServerSetup
    -> (String -> FilePath)
    -> ExperimentSetup
genBaselineExperimentSetup ecf runtime clients server signGlobally = ExperimentSetup
    { esRuntime = runtime
    , esResultsSummaryFile
        = experimentResultsDir ecf </> "summaries" </> signGlobally "summary" <.> jsonExt
    , esSetupFile
        = experimentResultsDir ecf </> "setups" </> signGlobally "setup" <.> jsonExt
    , clientSetups = clients
    , backendSetup = Left server
    }



