module AslBuild.Experiments where

import           Development.Shake

import           AslBuild.RunBaseLine
import           AslBuild.RunLocalExperiment

experimentRules :: Rules ()
experimentRules = do
    localExperimentRules
    baselineExperimentRules
