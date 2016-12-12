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

                let servers = genServerSetups sers
                let defaultClients = genClientSetup
                        ecf
                        cls
                        (RemoteServerUrl serPrivateIp . memcachedPort . sMemcachedFlags . (\[s] -> s) . servers)
                        signGlobally
                        blRuntime
                let clients = modif defaultClients $ \defcs -> flip map defcs $ \cs ->
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
                return $ genBaselineExperimentSetup ecf blRuntime clients ((\[s] -> s) . servers) signGlobally
        return (setups, vmsNeeded)

-- TODO Unify with the same thing in 'AslBuild.Experiment'
genBaselineExperimentSetup
    :: ExperimentConfig a
    => a
    -> TimeUnit
    -> (Int -> [ClientSetup])
    -> (Int -> ServerSetup)
    -> (String -> FilePath)
    -> [ExperimentSetup]
genBaselineExperimentSetup ecf runtime clients server signGlobally = flip map [1 .. repititions (highLevelConfig ecf)] $ \r ->
    ExperimentSetup
        { esRuntime = runtime
        , esResultsSummaryFile
            = experimentResultsDir ecf </> "summaries" </> signGlobally "summary" </> "rep-" ++ show r <.> jsonExt
        , esSetupFile
            = experimentResultsDir ecf </> "setups" </> signGlobally "setup" </> "rep-" ++ show r <.> jsonExt
        , clientSetups = clients r
        , backendSetup = Left $ server r
        }



