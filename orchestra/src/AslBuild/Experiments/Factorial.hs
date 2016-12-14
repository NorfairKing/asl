module AslBuild.Experiments.Factorial where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.Factorial.Types
import           AslBuild.Types

factorialRules :: Rules ()
factorialRules = mapM_ generateTargetFor allFactorialExperiments

allFactorialExperiments :: [FactorialCfg]
allFactorialExperiments =
    [ smallLocalFactorial
    , localFactorial
    , smallRemoteFactorial
    , remoteFactorial
    ]

smallLocalFactorial :: FactorialCfg
smallLocalFactorial = smallRemoteFactorial
    { hlConfig = (hlConfig smallRemoteFactorial)
        { target = "small-local-2k-factorial"
        , location = Local
        }
    }

localFactorial :: FactorialCfg
localFactorial = remoteFactorial
    { hlConfig = (hlConfig remoteFactorial)
        { target = "local-2k-factorial"
        , resultsPersistence = Volatile
        , location = Local
        }
    }

smallRemoteFactorial :: FactorialCfg
smallRemoteFactorial = remoteFactorial
    { hlConfig = (hlConfig remoteFactorial)
        { target = "small-remote-2k-factorial"
        , resultsPersistence = Volatile
        , repititions = 1
        }
    , fRuntime = Seconds 5
    }

remoteFactorial :: FactorialCfg
remoteFactorial = FactorialCfg
    { hlConfig = HighLevelConfig
        { target = "remote-2k-factorial"
        , nrClients = 1
        , nrServers = 1
        , location = Remote
        , resultsPersistence = Persistent
        , repititions = 3
        }
    , fRuntime = Minutes 1
    , virtualClients = (1, 128)
    , keySizes = (16, 128)
    , valueSizes = (128, 1024)
    }

