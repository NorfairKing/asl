module AslBuild.Models.MMm where

import           Development.Shake

import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.StabilityTrace

mmmRule :: String
mmmRule = "mmm-models"

mmmRules :: Rules ()
mmmRules = do
    mmmRule ~> need [ruleForStabilityTraces]

    subRules
        mmmRulesFor
        ruleForStabilityTraces
        allStabilityTraceExperiments

    subRules
        mmmRulesFor
        ruleForMaximumThroughputs
        allMaximumThroughputExperiments

ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-mmm-models"

ruleForMaximumThroughputs :: String
ruleForMaximumThroughputs = "maximum-throughput-mmm-models"

mmmRuleFor :: ExperimentConfig a => a -> String
mmmRuleFor ecf = experimentTarget ecf ++ "-mmm-model"

mmmRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
mmmRulesFor ecf = onlyIfResultsExist ecf $ do
    let mmmtarget = mmmRuleFor ecf
    mmmtarget ~> need []
    return mmmtarget
