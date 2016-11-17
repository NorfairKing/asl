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
smallRemoteWriteEffect = WriteEffectCfg
    { hlConfig = HighLevelConfig
        { target = smallRemoteWriteEffectRule
        , nrServers = 1
        , nrClients = 1
        , location = Remote
        , resultsPersistence = Volatile
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
        , nrClients = 1
        , location = Remote
        , resultsPersistence = Persistent
        }
    , serverCounts = [3 .. 7]
    , writePercentages = [0.01, 0.02 .. 0.1]
    , weRuntime = Minutes 1
    }
