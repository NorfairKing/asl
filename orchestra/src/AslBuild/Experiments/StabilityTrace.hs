module AslBuild.Experiments.StabilityTrace
    ( module AslBuild.Experiments.StabilityTrace
    , module AslBuild.Experiments.StabilityTrace.Types
    ) where

import           Development.Shake

import           AslBuild.Experiment
import           AslBuild.Experiments.StabilityTrace.Types
import           AslBuild.Middleware
import           AslBuild.Types

stabilityTraceRules :: Rules ()
stabilityTraceRules
    = mapM_ generateTargetFor allStabilityTraceExperiments

allStabilityTraceExperiments :: [StabilityTraceCfg]
allStabilityTraceExperiments =
    [ smallLocalStabilityTrace
    , localStabilityTrace
    , smallRemoteStabilityTrace
    , remoteStabilityTrace
    ]

smallLocalStabilityTraceRule :: String
smallLocalStabilityTraceRule = "small-local-stability-trace"

smallLocalStabilityTrace :: StabilityTraceCfg
smallLocalStabilityTrace = smallRemoteStabilityTrace
    { hlConfig = (hlConfig smallRemoteStabilityTrace)
        { target = smallLocalStabilityTraceRule
        , location = Local
        }
    }

localStabilityTracelRule :: String
localStabilityTracelRule = "local-stability-trace"

localStabilityTrace :: StabilityTraceCfg
localStabilityTrace = remoteStabilityTrace
    { hlConfig = (hlConfig remoteStabilityTrace)
        { target = localStabilityTracelRule
        , location = Local
        , resultsPersistence = Volatile
        }
    }

smallRemoteStabilityTraceRule :: String
smallRemoteStabilityTraceRule = "small-remote-stability-trace"

smallRemoteStabilityTrace :: StabilityTraceCfg
smallRemoteStabilityTrace = remoteStabilityTrace
    { hlConfig = (hlConfig remoteStabilityTrace)
        { target = smallRemoteStabilityTraceRule
        , nrServers = 1
        , nrClients = 1
        , resultsPersistence = Volatile
        , repititions = 2
        }
    , runtime = Seconds 10
    , logLevel = LogOff
    }


remoteStabilityTraceRule :: String
remoteStabilityTraceRule = "remote-stability-trace"

remoteStabilityTrace :: StabilityTraceCfg
remoteStabilityTrace = StabilityTraceCfg
    { hlConfig = HighLevelConfig
        { target = remoteStabilityTraceRule
        , nrServers = 3
        , nrClients = 3
        , location = Remote
        , resultsPersistence = Persistent
        , repititions = 3
        }
    , runtime = Hours 1
    , logLevel = LogOff
    }
