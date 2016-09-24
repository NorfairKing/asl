module AslBuild.Experiments where

import           AslBuild.OptParse
import           AslBuild.RunBaseLine
import           AslBuild.RunLocalExperiment

experimentRules :: AslBuilder ()
experimentRules = do
    localExperimentRules
    baselineExperimentRules
