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
    { target = smallLocalStabilityTraceRule
    , hlConfig = HighLevelConfig
        { nrServers = 1
        , nrClients = 1
        , location = Local
        }
    , runtime = Seconds 5
    , logLevel = LogFiner
    }

localStabilityTracelRule :: String
localStabilityTracelRule = "local-stability-trace"

localStabilityTrace :: StabilityTraceCfg
localStabilityTrace = StabilityTraceCfg
    { target = localStabilityTracelRule
    , hlConfig = (hlConfig remoteStabilityTrace) { location = Local }
    , runtime = runtime remoteStabilityTrace
    , logLevel = LogOff
    }

bigLocalStabilityTraceRule :: String
bigLocalStabilityTraceRule = "big-local-stability-trace"

bigLocalStabilityTrace :: StabilityTraceCfg
bigLocalStabilityTrace = StabilityTraceCfg
    { target = bigLocalStabilityTraceRule
    , hlConfig = HighLevelConfig
        { nrServers = 8
        , nrClients = 32
        , location = Local
        }
    , runtime = Hours 1
    , logLevel = LogOff
    }

smallRemoteStabilityTraceRule :: String
smallRemoteStabilityTraceRule = "small-remote-stability-trace"

smallRemoteStabilityTrace :: StabilityTraceCfg
smallRemoteStabilityTrace = StabilityTraceCfg
    { target = smallRemoteStabilityTraceRule
    , hlConfig = HighLevelConfig
        { nrServers = 3
        , nrClients = 3
        , location = Remote
        }
    , runtime = Seconds 10
    , logLevel = LogOff
    }


remoteStabilityTraceRule :: String
remoteStabilityTraceRule = "remote-stability-trace"

remoteStabilityTrace :: StabilityTraceCfg
remoteStabilityTrace = StabilityTraceCfg
    { target = remoteStabilityTraceRule
    , hlConfig = HighLevelConfig
        { nrServers = 3
        , nrClients = 3
        , location = Remote
        }
    , runtime = Hours 1
    , logLevel = LogOff
    }
