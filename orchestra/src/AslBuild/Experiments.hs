module AslBuild.Experiments where

import           Development.Shake

import           AslBuild.Experiments.Baseline
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace

experimentRules :: Rules ()
experimentRules = do
    baselineExperimentRules
    stabilityTraceRules
    replicationEffectRules
