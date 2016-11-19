module AslBuild.Experiments.ReplicationEffect
    ( module AslBuild.Experiments.ReplicationEffect
    , module AslBuild.Experiments.ReplicationEffect.Types
    ) where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.ReplicationEffect.Types
import           AslBuild.Types

replicationEffectRules :: Rules ()
replicationEffectRules = mapM_ generateTargetFor allReplicationEffectExperiments

allReplicationEffectExperiments :: [ReplicationEffectCfg]
allReplicationEffectExperiments =
    [ smallLocalReplicationEffect
    , localReplicationEffect
    , smallRemoteReplicationEffect
    , remoteReplicationEffect
    ]

smallLocalReplicationEffectRule :: String
smallLocalReplicationEffectRule = "small-local-replication-effect"

smallLocalReplicationEffect :: ReplicationEffectCfg
smallLocalReplicationEffect = smallRemoteReplicationEffect
    { hlConfig = (hlConfig smallRemoteReplicationEffect)
        { target = smallLocalReplicationEffectRule
        , location = Local
        }
    }

localReplicationEffectRule :: String
localReplicationEffectRule = "local-replication-effect"

localReplicationEffect :: ReplicationEffectCfg
localReplicationEffect = remoteReplicationEffect
    { hlConfig = (hlConfig remoteReplicationEffect)
        { target = localReplicationEffectRule
        , location = Local
        , resultsPersistence = Volatile
        }
    }

smallRemoteReplicationEffectRule :: String
smallRemoteReplicationEffectRule = "small-remote-replication-effect"

smallRemoteReplicationEffect :: ReplicationEffectCfg
smallRemoteReplicationEffect = ReplicationEffectCfg
    { hlConfig = HighLevelConfig
        { target = smallRemoteReplicationEffectRule
        , nrServers = 1
        , nrClients = 1
        , location = Remote
        , resultsPersistence = Volatile
        }
    , serverCounts = [1]
    , replicationFactors = [0]
    , reRuntime = Seconds 5
    }


remoteReplicationEffectRule :: String
remoteReplicationEffectRule = "remote-replication-effect"

remoteReplicationEffect :: ReplicationEffectCfg
remoteReplicationEffect = ReplicationEffectCfg
    { hlConfig = HighLevelConfig
        { target = remoteReplicationEffectRule
        , nrServers = 7
        , nrClients = 2
        , location = Remote
        , resultsPersistence = Persistent
        }
    , serverCounts = [3, 5, 7]
    , replicationFactors = [0, 0.5, 1]
    , reRuntime = Minutes 1
    }
