module AslBuild.Experiments where

import           Development.Shake

import           AslBuild.Experiments.Baseline
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.WriteEffect

experimentRules :: Rules ()
experimentRules = do
    baselineExperimentRules
    replicationEffectRules
    stabilityTraceRules
    writeEffectRules
    maximumThroughputRules
