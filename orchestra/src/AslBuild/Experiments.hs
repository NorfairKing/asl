module AslBuild.Experiments where

import           Development.Shake

import           AslBuild.Experiments.Baseline
import           AslBuild.Experiments.StabilityTrace

experimentRules :: Rules ()
experimentRules = do
    baselineExperimentRules
    stabilityTraceRules
