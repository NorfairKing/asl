module AslBuild.Models.MM1 where

import           Control.Monad
import           Control.Monad.IO.Class

import           Development.Shake

import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Utils

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
    slocs <- readResultsSummaryLocationsForCfg ecf
    mm1ModelFiles <- forM slocs $ \sloc -> do
        let modelFile = mm1MiddlewareModelFileFor ecf sloc
        modelFile %> \outf -> do
            mm1Model <- calcMiddlewareMM1Model sloc
            liftIO $ print mm1Model
            writeJSON outf mm1Model
        return modelFile

    let mm1target = mm1RuleFor ecf
    mm1target ~> need mm1ModelFiles
    return mm1target

