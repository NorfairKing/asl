module AslBuild.Models.MM1 where

import           Control.Monad

import           Development.Shake

import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Utils

import           AslBuild.Models.MM1.Clients
import           AslBuild.Models.MM1.Middleware

mm1Rule :: String
mm1Rule = "mm1-models"

mm1Rules :: Rules ()
mm1Rules = do
    mm1Rule ~> need [ruleForStabilityTraces]

    subRules
        mm1RulesFor
        ruleForStabilityTraces
        allStabilityTraceExperiments

ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-mm1-models"

mm1RuleFor :: ExperimentConfig a => a -> String
mm1RuleFor ecf = experimentTarget ecf ++ "-mm1-model"

mm1RulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
mm1RulesFor ecf = onlyIfResultsExist ecf $ do
    middleRule <- mm1MiddlewareRulesFor ecf
    clientsRule <- mm1ClientsRulesFor ecf

    let mm1target = mm1RuleFor ecf
    mm1target ~> need [middleRule, clientsRule]
    return mm1target

mm1MiddlewareRuleFor :: ExperimentConfig a => a -> String
mm1MiddlewareRuleFor ecf = experimentTarget ecf ++ "-middleware-mm1-model"

mm1MiddlewareRulesFor :: ExperimentConfig a => a -> Rules String
mm1MiddlewareRulesFor ecf = do
    slocs <- readResultsSummaryLocationsForCfg ecf
    mm1ModelFiles <- forM slocs $ \sloc -> do
        let modelFile = mm1MiddlewareModelFileFor ecf sloc
        modelFile %> \outf -> do
            mm1Model <- calcMiddlewareMM1Model sloc
            writeJSON outf mm1Model
        return modelFile

    let mm1target = mm1MiddlewareRuleFor ecf
    mm1target ~> need mm1ModelFiles
    return mm1target

mm1ClientsRuleFor :: ExperimentConfig a => a -> String
mm1ClientsRuleFor ecf = experimentTarget ecf ++ "-clients-mm1-model"

mm1ClientsRulesFor :: ExperimentConfig a => a -> Rules String
mm1ClientsRulesFor ecf = do
    slocs <- readResultsSummaryLocationsForCfg ecf
    mm1ModelFiles <- forM slocs $ \sloc -> do
        let modelFile = mm1ClientsModelFileFor ecf sloc
        modelFile %> \outf -> do
            mm1Model <- calcClientsMM1Model ecf sloc
            writeJSON outf mm1Model
        return modelFile

    let mm1target = mm1ClientsRuleFor ecf
    mm1target ~> need mm1ModelFiles
    return mm1target
