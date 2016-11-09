module AslBuild.Experiments.MinimumThroughput
    ( module AslBuild.Experiments.MinimumThroughput
    , module AslBuild.Experiments.MinimumThroughput.Types
    ) where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.MinimumThroughput.Types
import           AslBuild.Types

minimumThroughputRules :: Rules ()
minimumThroughputRules = do
    generateTargetFor smallLocalMinimumThroughput
    generateTargetFor localMinimumThroughput
    generateTargetFor heavyLocalMinimumThroughput
    generateTargetFor smallRemoteMinimumThroughput
    generateTargetFor remoteMinimumThroughput

smallLocalMinimumThroughputRule :: String
smallLocalMinimumThroughputRule = "small-local-minimum-throughput"

smallLocalMinimumThroughput :: MinimumThroughputCfg
smallLocalMinimumThroughput = smallRemoteMinimumThroughput
    { hlConfig = (hlConfig smallRemoteMinimumThroughput)
        { target = smallLocalMinimumThroughputRule
        , location = Local
        , resultsPersistence = Volatile
        }
    }

localMinimumThroughputRule :: String
localMinimumThroughputRule = "local-minimum-throughput"

localMinimumThroughput :: MinimumThroughputCfg
localMinimumThroughput = remoteMinimumThroughput
    { hlConfig = (hlConfig remoteMinimumThroughput)
        { target = localMinimumThroughputRule
        , location = Local
        }
    }

heavyLocalMinimumThroughputRule :: String
heavyLocalMinimumThroughputRule = "heavy-local-minimum-throughput"

heavyLocalMinimumThroughput :: MinimumThroughputCfg
heavyLocalMinimumThroughput = MinimumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = heavyLocalMinimumThroughputRule
        , nrServers = 1
        , nrClients = 8
        , location = Local
        , resultsPersistence = Volatile
        }
    , threadConcTups = [(1, 500)]
    , mtRuntime = Seconds 30
    }

smallRemoteMinimumThroughputRule :: String
smallRemoteMinimumThroughputRule = "small-remote-minimum-throughput"

smallRemoteMinimumThroughput :: MinimumThroughputCfg
smallRemoteMinimumThroughput = MinimumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = smallRemoteMinimumThroughputRule
        , nrServers = 2
        , nrClients = 1
        , location = Remote
        , resultsPersistence = Persistent
        }
    , threadConcTups = do
        middleThreads <- [1, 2]
        concurrencies <- [5, 10]
        return (middleThreads, concurrencies)
    , mtRuntime = Seconds 10
    }


remoteMinimumThroughputRule :: String
remoteMinimumThroughputRule = "remote-minimum-throughput"

remoteMinimumThroughput :: MinimumThroughputCfg
remoteMinimumThroughput = MinimumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = remoteMinimumThroughputRule
        , nrServers = 5
        , nrClients = 3
        , location = Remote
        , resultsPersistence = Persistent
        }
    , threadConcTups = do
        middleThreads <- [2, 4 .. 8]
        concurrencies <- [10, 20 .. 100]
        return (middleThreads, concurrencies)
    , mtRuntime = Minutes 1
    }
