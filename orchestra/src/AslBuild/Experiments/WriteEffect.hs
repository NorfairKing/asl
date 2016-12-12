module AslBuild.Experiments.WriteEffect
    ( module AslBuild.Experiments.WriteEffect
    , module AslBuild.Experiments.WriteEffect.Types
    ) where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.WriteEffect.Types
import           AslBuild.Types

writeEffectRules :: Rules ()
writeEffectRules = mapM_ generateTargetFor allWriteEffectExperiments

allWriteEffectExperiments :: [WriteEffectCfg]
allWriteEffectExperiments =
    [ smallLocalWriteEffect
    , localWriteEffect
    , smallRemoteWriteEffect
    , remoteWriteEffect
    ]

smallLocalWriteEffectRule :: String
smallLocalWriteEffectRule = "small-local-write-effect"

smallLocalWriteEffect :: WriteEffectCfg
smallLocalWriteEffect = smallRemoteWriteEffect
    { hlConfig = (hlConfig smallRemoteWriteEffect)
        { target = smallLocalWriteEffectRule
        , location = Local
        }
    }

localWriteEffectRule :: String
localWriteEffectRule = "local-write-effect"

localWriteEffect :: WriteEffectCfg
localWriteEffect = remoteWriteEffect
    { hlConfig = (hlConfig remoteWriteEffect)
        { target = localWriteEffectRule
        , location = Local
        , resultsPersistence = Volatile
        }
    }

smallRemoteWriteEffectRule :: String
smallRemoteWriteEffectRule = "small-remote-write-effect"

smallRemoteWriteEffect :: WriteEffectCfg
smallRemoteWriteEffect = remoteWriteEffect
    { hlConfig = (hlConfig remoteWriteEffect)
        { target = smallRemoteWriteEffectRule
        , nrServers = 1
        , nrClients = 1
        , resultsPersistence = Volatile
        , repititions = 1
        }
    , serverCounts = [1]
    , writePercentages = [0.01]
    , weRuntime = Seconds 10
    }


remoteWriteEffectRule :: String
remoteWriteEffectRule = "remote-write-effect"

remoteWriteEffect :: WriteEffectCfg
remoteWriteEffect = WriteEffectCfg
    { hlConfig = HighLevelConfig
        { target = remoteWriteEffectRule
        , nrServers = 7
        , nrClients = 2
        , location = Remote
        , resultsPersistence = Persistent
        , repititions = 3
        }
    , serverCounts = [3, 5, 7] -- [3 .. 7]
    , writePercentages = [0.01, 0.05, 0.1] -- map (* 0.01) [1 .. 10]
    , weRuntime = Minutes 1
    }
