module AslBuild.Experiments where

import Development.Shake

import AslBuild.Experiments.Baseline
import AslBuild.Experiments.Extreme
import AslBuild.Experiments.Factorial
import AslBuild.Experiments.MaximumThroughput
import AslBuild.Experiments.ReplicationEffect
import AslBuild.Experiments.StabilityTrace
import AslBuild.Experiments.ThinkTime
import AslBuild.Experiments.WriteEffect

experimentRules :: Rules ()
experimentRules = do
    baselineExperimentRules
    replicationEffectRules
    stabilityTraceRules
    writeEffectRules
    maximumThroughputRules
    thinkTimeRules
    factorialRules
    extremeRules
