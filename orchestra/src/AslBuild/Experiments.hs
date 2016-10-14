module AslBuild.Experiments where

import           Development.Shake

import           AslBuild.Baseline
import           AslBuild.StabilityTrace

experimentRules :: Rules ()
experimentRules = do
    baselineExperimentRules
    stabilityTraceRules
