module AslBuild.Models.MMm where

import           Control.Monad

import           Development.Shake

import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Utils

import           AslBuild.Models.MMm.Middleware

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
    middleRule <- mmmmiddlewareRulesFor ecf

    let mmmtarget = mmmRuleFor ecf
    mmmtarget ~> need [middleRule]
    return mmmtarget

mmmmiddlewareRuleFor :: ExperimentConfig a => a -> String
mmmmiddlewareRuleFor ecf = experimentTarget ecf ++ "-middleware-mmm-model"

mmmmiddlewareRulesFor :: ExperimentConfig a => a -> Rules String
mmmmiddlewareRulesFor ecf = do
    slocs <- readResultsSummaryLocationsForCfg ecf
    mmmModelFiles <- forM slocs $ \sloc -> do
        let modelFile = mmmMiddlewareModelFileFor ecf sloc
        modelFile %> \outf -> do
            mmmModel <- calcMiddlewareMMmModel sloc
            writeJSON outf mmmModel
        return modelFile

    let mmmtarget = mmmmiddlewareRuleFor ecf
    mmmtarget ~> need mmmModelFiles
    return mmmtarget
