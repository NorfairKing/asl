module AslBuild.Experiments.WriteEffect
    ( module AslBuild.Experiments.WriteEffect
    , module AslBuild.Experiments.WriteEffect.Types
    ) where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.WriteEffect.Types
import           AslBuild.Types

writeEffectRules :: Rules ()
writeEffectRules = do
    generateTargetFor smallLocalWriteEffect
    generateTargetFor localWriteEffect
    generateTargetFor smallRemoteWriteEffect
    generateTargetFor remoteWriteEffect

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
        }
    }

smallRemoteWriteEffectRule :: String
smallRemoteWriteEffectRule = "small-remote-write-effect"

smallRemoteWriteEffect :: WriteEffectCfg
smallRemoteWriteEffect = WriteEffectCfg
    { hlConfig = HighLevelConfig
        { target = smallRemoteWriteEffectRule
        , nrServers = 2
        , nrClients = 1
        , location = Remote
        , resultsPersistence = Volatile
        }
    , serverCounts = [1, 2]
    , writePercentages = [0.01, 0.1]
    }


remoteWriteEffectRule :: String
remoteWriteEffectRule = "remote-write-effect"

remoteWriteEffect :: WriteEffectCfg
remoteWriteEffect = WriteEffectCfg
    { hlConfig = HighLevelConfig
        { target = remoteWriteEffectRule
        , nrServers = 7
        , nrClients = 3
        , location = Remote
        , resultsPersistence = Persistent
        }
    , serverCounts = [3, 5, 7]
    , writePercentages = [0.01, 0.05, 0.1]
    }
