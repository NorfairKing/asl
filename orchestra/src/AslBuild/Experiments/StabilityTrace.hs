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
stabilityTraceRules = do
    generateTargetFor smallLocalStabilityTrace
    generateTargetFor localStabilityTrace
    generateTargetFor bigLocalStabilityTrace
    generateTargetFor smallRemoteStabilityTrace
    generateTargetFor remoteStabilityTrace

smallLocalStabilityTraceRule :: String
smallLocalStabilityTraceRule = "small-local-stability-trace"

smallLocalStabilityTrace :: StabilityTraceCfg
smallLocalStabilityTrace = StabilityTraceCfg
    { hlConfig = (hlConfig smallRemoteStabilityTrace)
        { target = smallLocalStabilityTraceRule
        , location = Local
        }
    , runtime = Seconds 5
    , logLevel = LogFiner
    }

localStabilityTracelRule :: String
localStabilityTracelRule = "local-stability-trace"

localStabilityTrace :: StabilityTraceCfg
localStabilityTrace = StabilityTraceCfg
    { hlConfig = (hlConfig remoteStabilityTrace)
        { target = localStabilityTracelRule
        , location = Local
        }
    , runtime = runtime remoteStabilityTrace
    , logLevel = LogOff
    }

bigLocalStabilityTraceRule :: String
bigLocalStabilityTraceRule = "big-local-stability-trace"

bigLocalStabilityTrace :: StabilityTraceCfg
bigLocalStabilityTrace = StabilityTraceCfg
    { hlConfig = HighLevelConfig
        { target = bigLocalStabilityTraceRule
        , nrServers = 8
        , nrClients = 32
        , location = Local
        , resultsPersistence = Persistent
        }
    , runtime = Hours 1
    , logLevel = LogOff
    }

smallRemoteStabilityTraceRule :: String
smallRemoteStabilityTraceRule = "small-remote-stability-trace"

smallRemoteStabilityTrace :: StabilityTraceCfg
smallRemoteStabilityTrace = StabilityTraceCfg
    { hlConfig = HighLevelConfig
        { target = smallRemoteStabilityTraceRule
        , nrServers = 3
        , nrClients = 3
        , location = Remote
        , resultsPersistence = Volatile
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
        }
    , runtime = Hours 1
    , logLevel = LogOff
    }
