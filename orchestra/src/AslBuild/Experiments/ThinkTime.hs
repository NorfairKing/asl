module AslBuild.Experiments.ThinkTime where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.ThinkTime.Types
import           AslBuild.Types

thinkTimeRules :: Rules ()
thinkTimeRules = mapM_ generateTargetFor allThinkTimeExperiments

allThinkTimeExperiments :: [ThinkTimeCfg]
allThinkTimeExperiments =
    [ smallLocalThinkTimeExperiment
    , localThinkTimeExperiment
    , smallRemoteThinkTimeExperiment
    , remoteThinkTimeExperiment
    ]

smallLocalThinkTimeExperiment :: ThinkTimeCfg
smallLocalThinkTimeExperiment = smallRemoteThinkTimeExperiment
    { hlConfig = (hlConfig smallRemoteThinkTimeExperiment)
        { target = "small-local-think-time"
        , location = Local
        }
    }


localThinkTimeExperiment :: ThinkTimeCfg
localThinkTimeExperiment = remoteThinkTimeExperiment
    { hlConfig = (hlConfig remoteThinkTimeExperiment)
        { target = "local-think-time"
        , resultsPersistence = Volatile
        , location = Local
        }
    }

smallRemoteThinkTimeExperiment :: ThinkTimeCfg
smallRemoteThinkTimeExperiment = remoteThinkTimeExperiment
    { hlConfig = (hlConfig remoteThinkTimeExperiment)
        { target = "small-remote-think-time"
        , resultsPersistence = Volatile
        }
    , ttRuntime = Seconds 10
    }

remoteThinkTimeExperiment :: ThinkTimeCfg
remoteThinkTimeExperiment = ThinkTimeCfg
    { hlConfig = HighLevelConfig
        { target = "remote-think-time"
        , nrClients = 1
        , nrServers = 1
        , location = Remote
        , resultsPersistence = Volatile
        }
    , ttRuntime = Minutes 1
    }
