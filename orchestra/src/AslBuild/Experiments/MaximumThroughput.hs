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
    , bigLocalMaximumThroughput
    , smallRemoteMaximumThroughput
    , remoteMaximumThroughput
    , bigRemoteMaximumThroughput
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

bigLocalMaximumThroughputRule :: String
bigLocalMaximumThroughputRule = "big-local-maximum-throughput"

bigLocalMaximumThroughput :: MaximumThroughputCfg
bigLocalMaximumThroughput = bigRemoteMaximumThroughput
    { hlConfig = (hlConfig bigRemoteMaximumThroughput)
        { target = bigLocalMaximumThroughputRule
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
        concurrencies <- [8]
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
        concurrencies <- [1..25]
        return (middleThreads, concurrencies)
    , mtRuntime = Seconds 30
    }

bigRemoteMaximumThroughputRule :: String
bigRemoteMaximumThroughputRule = "big-remote-maximum-throughput"

bigRemoteMaximumThroughput :: MaximumThroughputCfg
bigRemoteMaximumThroughput = MaximumThroughputCfg
    { hlConfig = HighLevelConfig
        { target = bigRemoteMaximumThroughputRule
        , nrServers = 5
        , nrClients = 3
        , location = Remote
        , resultsPersistence = Persistent
        }
    , threadConcTups = do
        middleThreads <- [1, 6 .. 30]
        concurrencies <- [1, 6 .. 125]
        return (middleThreads, concurrencies)
    , mtRuntime = Seconds 30
    }
