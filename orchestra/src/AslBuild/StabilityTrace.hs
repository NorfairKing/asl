module AslBuild.StabilityTrace where

import           Development.Shake

import           AslBuild.Experiment
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

smallLocalStabilityTrace :: ExperimentCfg
smallLocalStabilityTrace = ExperimentCfg
    { target = smallLocalStabilityTraceRule
    , nrServers = 1
    , nrClients = 1
    , location = Local
    , runtime = Seconds 5
    , logLevel = LogFiner
    }

localStabilityTracelRule :: String
localStabilityTracelRule = "local-stability-trace"

localStabilityTrace :: ExperimentCfg
localStabilityTrace = ExperimentCfg
    { target = localStabilityTracelRule
    , nrServers = nrServers remoteStabilityTrace
    , nrClients = nrClients remoteStabilityTrace
    , location = Local
    , runtime = runtime remoteStabilityTrace
    , logLevel = LogOff
    }

bigLocalStabilityTraceRule :: String
bigLocalStabilityTraceRule = "big-local-stability-trace"

bigLocalStabilityTrace :: ExperimentCfg
bigLocalStabilityTrace = ExperimentCfg
    { target = bigLocalStabilityTraceRule
    , nrServers = 8
    , nrClients = 32
    , location = Local
    , runtime = Hours 1
    , logLevel = LogOff
    }

smallRemoteStabilityTraceRule :: String
smallRemoteStabilityTraceRule = "small-remote-stability-trace"

smallRemoteStabilityTrace :: ExperimentCfg
smallRemoteStabilityTrace = ExperimentCfg
    { target = smallRemoteStabilityTraceRule
    , nrServers = 3
    , nrClients = 3
    , location = Remote
    , runtime = Seconds 10
    , logLevel = LogOff
    }


remoteStabilityTraceRule :: String
remoteStabilityTraceRule = "remote-stability-trace"

remoteStabilityTrace :: ExperimentCfg
remoteStabilityTrace = ExperimentCfg
    { target = remoteStabilityTraceRule
    , nrServers = 3
    , nrClients = 3
    , location = Remote
    , runtime = Hours 1
    , logLevel = LogOff
    }
