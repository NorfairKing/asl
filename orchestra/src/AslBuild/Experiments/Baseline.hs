{-# LANGUAGE OverloadedStrings #-}

module AslBuild.Experiments.Baseline
    ( module AslBuild.Experiments.Baseline
    , module AslBuild.Experiments.Baseline.Types
    ) where

import Development.Shake

import AslBuild.Experiment
import AslBuild.Experiments.Baseline.Types
import AslBuild.Types

baselineExperimentRules :: Rules ()
baselineExperimentRules = mapM_ generateTargetFor allBaselineExperiments

allBaselineExperiments :: [BaselineExperimentRuleCfg]
allBaselineExperiments =
    [ smallLocalBaselineExperiment
    , localBaselineExperiment
    , smallRemoteBaselineExperiment
    , remoteBaselineExperiment
    ]

smallLocalBaselineExperimentRule :: String
smallLocalBaselineExperimentRule = "small-local-baseline-experiment"

smallLocalBaselineExperiment :: BaselineExperimentRuleCfg
smallLocalBaselineExperiment =
    smallRemoteBaselineExperiment
    { hlConfig =
          (hlConfig smallRemoteBaselineExperiment)
          {target = smallLocalBaselineExperimentRule, location = Local}
    }

localBaselineExperimentRule :: String
localBaselineExperimentRule = "local-baseline-experiment"

localBaselineExperiment :: BaselineExperimentRuleCfg
localBaselineExperiment =
    remoteBaselineExperiment
    { hlConfig =
          (hlConfig remoteBaselineExperiment)
          {target = localBaselineExperimentRule, location = Local, resultsPersistence = Volatile}
    }

smallRemoteBaselineExperimentRule :: String
smallRemoteBaselineExperimentRule = "small-remote-baseline-experiment"

smallRemoteBaselineExperiment :: BaselineExperimentRuleCfg
smallRemoteBaselineExperiment =
    remoteBaselineExperiment
    { hlConfig =
          (hlConfig remoteBaselineExperiment)
          { target = smallRemoteBaselineExperimentRule
          , nrServers = 1
          , nrClients = 1
          , resultsPersistence = Volatile
          , repititions = 1
          }
    , blRuntime = Seconds 5
    , concurrencies = [1]
    , repetitions = 1
    }

remoteBaselineExperimentRule :: String
remoteBaselineExperimentRule = "remote-baseline-experiment"

remoteBaselineExperiment :: BaselineExperimentRuleCfg
remoteBaselineExperiment =
    BaselineExperimentRuleCfg
    { hlConfig =
          HighLevelConfig
          { target = remoteBaselineExperimentRule
          , nrServers = 1
          , nrClients = 2
          , location = Remote
          , resultsPersistence = Persistent
          , repititions = 3
          }
    , blRuntime = Seconds 30
    , concurrencies = [1,5 .. 128]
    , repetitions = 5
    }
