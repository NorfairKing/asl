module AslBuild.Experiments.MaximumThroughput
    ( module AslBuild.Experiments.MaximumThroughput
    , module AslBuild.Experiments.MaximumThroughput.Types
    ) where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput.Types
import           AslBuild.Types

maximumThroughputRules :: Rules ()
maximumThroughputRules = do
    generateTargetFor smallLocalMaximumThroughput
    generateTargetFor localMaximumThroughput
    generateTargetFor bigLocalMaximumThroughput
    generateTargetFor heavyLocalMaximumThroughput
    generateTargetFor smallRemoteMaximumThroughput
    generateTargetFor remoteMaximumThroughput

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
        }
    }

bigLocalMaximumThroughputRule :: String
bigLocalMaximumThroughputRule = "big-local-maximum-throughput"

bigLocalMaximumThroughput :: MaximumThroughputCfg
bigLocalMaximumThroughput = MaximumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = bigLocalMaximumThroughputRule
        , nrServers = 8
        , nrClients = 32
        , location = Local
        , resultsPersistence = Persistent
        }
    , threadConcTups = do
        middleThreads <- [1 .. 8]
        concurrencies <- [5, 10 .. 60]
        return (middleThreads, concurrencies)
    }

heavyLocalMaximumThroughputRule :: String
heavyLocalMaximumThroughputRule = "heavy-local-maximum-throughput"

heavyLocalMaximumThroughput :: MaximumThroughputCfg
heavyLocalMaximumThroughput = MaximumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = heavyLocalMaximumThroughputRule
        , nrServers = 1
        , nrClients = 8
        , location = Local
        , resultsPersistence = Volatile
        }
    , threadConcTups = [(1, 8)]
    }

smallRemoteMaximumThroughputRule :: String
smallRemoteMaximumThroughputRule = "small-remote-maximum-throughput"

smallRemoteMaximumThroughput :: MaximumThroughputCfg
smallRemoteMaximumThroughput = MaximumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = smallRemoteMaximumThroughputRule
        , nrServers = 2
        , nrClients = 1
        , location = Remote
        , resultsPersistence = Persistent
        }
    , threadConcTups = do
        middleThreads <- [1, 2]
        concurrencies <- [5, 10]
        return (middleThreads, concurrencies)
    }


remoteMaximumThroughputRule :: String
remoteMaximumThroughputRule = "remote-maximum-throughput"

remoteMaximumThroughput :: MaximumThroughputCfg
remoteMaximumThroughput = MaximumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = remoteMaximumThroughputRule
        , nrServers = 5
        , nrClients = 3
        , location = Remote
        , resultsPersistence = Persistent
        }
    , threadConcTups = do
        middleThreads <- [2, 4 .. 8]
        concurrencies <- [10, 20 .. 100]
        return (middleThreads, concurrencies)
    }
