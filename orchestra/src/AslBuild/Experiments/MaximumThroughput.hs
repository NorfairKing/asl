module AslBuild.Experiments.MaximumThroughput
    ( module AslBuild.Experiments.MaximumThroughput
    , module AslBuild.Experiments.MaximumThroughput.Types
    ) where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput.Types
import           AslBuild.Types

maximumThroughputRules :: Rules ()
maximumThroughputRules = mapM_ generateTargetFor allMaximumThroughputExperiments

allMaximumThroughputExperiments :: [MaximumThroughputCfg]
allMaximumThroughputExperiments =
    [ smallLocalMaximumThroughput
    , localMaximumThroughput
    , smallRemoteMaximumThroughput
    , remoteMaximumThroughput
    ]

smallLocalMaximumThroughputRule :: String
smallLocalMaximumThroughputRule = "small-local-maximum-throughput"

smallLocalMaximumThroughput :: MaximumThroughputCfg
smallLocalMaximumThroughput = smallRemoteMaximumThroughput
    { hlConfig = (hlConfig smallRemoteMaximumThroughput)
        { target = smallLocalMaximumThroughputRule
        , location = Local
        , resultsPersistence = Volatile
        }
    }

localMaximumThroughputRule :: String
localMaximumThroughputRule = "local-maximum-throughput"

localMaximumThroughput :: MaximumThroughputCfg
localMaximumThroughput = remoteMaximumThroughput
    { hlConfig = (hlConfig remoteMaximumThroughput)
        { target = localMaximumThroughputRule
        , location = Local
        , resultsPersistence = Volatile
        }
    }

smallRemoteMaximumThroughputRule :: String
smallRemoteMaximumThroughputRule = "small-remote-maximum-throughput"

smallRemoteMaximumThroughput :: MaximumThroughputCfg
smallRemoteMaximumThroughput = MaximumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = smallRemoteMaximumThroughputRule
        , nrServers = 1
        , nrClients = 1
        , location = Remote
        , resultsPersistence = Volatile
        }
    , threadConcTups = do
        middleThreads <- [1]
        concurrencies <- [8, 16]
        return (middleThreads, concurrencies)
    , mtRuntime = Seconds 10
    }


remoteMaximumThroughputRule :: String
remoteMaximumThroughputRule = "remote-maximum-throughput"

remoteMaximumThroughput :: MaximumThroughputCfg
remoteMaximumThroughput = MaximumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = remoteMaximumThroughputRule
        , nrServers = 5
        , nrClients = 1
        , location = Remote
        , resultsPersistence = Persistent
        }
    , threadConcTups = do
        middleThreads <- [1..6]
        concurrencies <-
            scanl (+) 1 $
                concatMap (uncurry replicate)
                [(9, 1), (5, 2), (3, 3), (2, 4)]
        return (middleThreads, concurrencies)
    , mtRuntime = Minutes 1
    }
