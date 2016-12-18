module AslBuild.Experiments.Extreme where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.Extreme.Types
import           AslBuild.Types

extremeRules :: Rules ()
extremeRules = mapM_ generateTargetFor allExtremeExperiments

allExtremeExperiments :: [ExtremeCfg]
allExtremeExperiments =
    [ smallLocalExtreme
    , localExtreme
    , smallRemoteExtreme
    , remoteExtreme
    ]

smallLocalExtreme :: ExtremeCfg
smallLocalExtreme = smallRemoteExtreme
    { hlConfig = (hlConfig smallRemoteExtreme)
        { target = "small-local-extreme"
        , location = Local
        }
    }

localExtreme :: ExtremeCfg
localExtreme = remoteExtreme
    { hlConfig = (hlConfig remoteExtreme)
        { target = "local-extreme"
        , resultsPersistence = Volatile
        , location = Local
        }
    }

smallRemoteExtreme :: ExtremeCfg
smallRemoteExtreme = remoteExtreme
    { hlConfig = (hlConfig remoteExtreme)
        { target = "small-remote-extreme"
        , resultsPersistence = Volatile
        , repititions = 1
        }
    , exRuntime = Seconds 5
    }

remoteExtreme :: ExtremeCfg
remoteExtreme = ExtremeCfg
    { hlConfig = HighLevelConfig
        { target = "remote-extreme"
        , nrClients = 3
        , nrServers = 7
        , location = Remote
        , resultsPersistence = Persistent
        , repititions = 5
        }
    , exRuntime = Minutes 3
    }

