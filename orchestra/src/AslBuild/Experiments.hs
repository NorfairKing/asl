module AslBuild.Experiments where

import           Development.Shake

import           AslBuild.Baseline
import           AslBuild.LocalLogTest

experimentRules :: Rules ()
experimentRules = do
    localLogTestRules
    baselineExperimentRules
