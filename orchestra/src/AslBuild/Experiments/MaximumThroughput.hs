module AslBuild.Experiments.MaximumThroughput
    ( module AslBuild.Experiments.MaximumThroughput
    , module AslBuild.Experiments.MaximumThroughput.Types
    ) where

import Development.Shake

import AslBuild.Experiment
import AslBuild.Experiments.MaximumThroughput.Types
import AslBuild.Types

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
smallLocalMaximumThroughput =
    smallRemoteMaximumThroughput
    { hlConfig =
          (hlConfig smallRemoteMaximumThroughput)
          { target = smallLocalMaximumThroughputRule
          , location = Local
          , resultsPersistence = Volatile
          }
    }

localMaximumThroughputRule :: String
localMaximumThroughputRule = "local-maximum-throughput"

localMaximumThroughput :: MaximumThroughputCfg
localMaximumThroughput =
    remoteMaximumThroughput
    { hlConfig =
          (hlConfig remoteMaximumThroughput)
          {target = localMaximumThroughputRule, location = Local, resultsPersistence = Volatile}
    }

smallRemoteMaximumThroughputRule :: String
smallRemoteMaximumThroughputRule = "small-remote-maximum-throughput"

smallRemoteMaximumThroughput :: MaximumThroughputCfg
smallRemoteMaximumThroughput =
    remoteMaximumThroughput
    { hlConfig =
          (hlConfig remoteMaximumThroughput)
          { target = smallRemoteMaximumThroughputRule
          , nrServers = 1
          , nrClients = 1
          , resultsPersistence = Volatile
          , repititions = 1
          }
    , threadConcTups =
          do middleThreads <- [1]
             concurrencies <- [8]
             return (middleThreads, concurrencies)
    , mtRuntime = Seconds 10
    }

remoteMaximumThroughputRule :: String
remoteMaximumThroughputRule = "remote-maximum-throughput"

remoteMaximumThroughput :: MaximumThroughputCfg
remoteMaximumThroughput =
    MaximumThroughputCfg
    { hlConfig =
          HighLevelConfig
          { target = remoteMaximumThroughputRule
          , nrServers = 5
          , nrClients = 2
          , location = Remote
          , resultsPersistence = Persistent
          , repititions = 3
          }
    , threadConcTups =
          do middleThreads <- [1,5 .. 21]
             concurrencies <- [1,6 .. 100]
             return (middleThreads, concurrencies)
    , mtRuntime = Minutes 1
    }
