module AslBuild.Experiments.ThinkTime
    ( module AslBuild.Experiments.ThinkTime
    , module AslBuild.Experiments.ThinkTime.Types
    ) where

import Development.Shake

import AslBuild.Experiment
import AslBuild.Experiments.ThinkTime.Types
import AslBuild.Types

thinkTimeRules :: Rules ()
thinkTimeRules = mapM_ generateTargetFor allThinkTimeExperiments

allThinkTimeExperiments :: [ThinkTimeCfg]
allThinkTimeExperiments =
    [smallLocalThinkTime, localThinkTime, smallRemoteThinkTime, remoteThinkTime]

smallLocalThinkTime :: ThinkTimeCfg
smallLocalThinkTime =
    smallRemoteThinkTime
    { hlConfig =
          (hlConfig smallRemoteThinkTime) {target = "small-local-think-time", location = Local}
    }

localThinkTime :: ThinkTimeCfg
localThinkTime =
    remoteThinkTime
    { hlConfig =
          (hlConfig remoteThinkTime)
          {target = "local-think-time", resultsPersistence = Volatile, location = Local}
    }

smallRemoteThinkTime :: ThinkTimeCfg
smallRemoteThinkTime =
    remoteThinkTime
    { hlConfig =
          (hlConfig remoteThinkTime)
          {target = "small-remote-think-time", resultsPersistence = Volatile, repititions = 1}
    , ttRuntime = Seconds 10
    }

remoteThinkTime :: ThinkTimeCfg
remoteThinkTime =
    ThinkTimeCfg
    { hlConfig =
          HighLevelConfig
          { target = "remote-think-time"
          , nrClients = 1
          , nrServers = 1
          , location = Remote
          , resultsPersistence = Persistent
          , repititions = 3
          }
    , ttRuntime = Seconds 30
    }
