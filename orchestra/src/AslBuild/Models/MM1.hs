module AslBuild.Models.MM1 where

import           Control.Monad
import           Control.Monad.IO.Class
import           Text.Printf

import           Development.Shake

import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Reports.Utils
import           AslBuild.Utils

import           AslBuild.Models.MM1.Clients
import           AslBuild.Models.MM1.Middleware
import           AslBuild.Models.MM1.Report
import           AslBuild.Models.MM1.Types
import           AslBuild.Models.MM1.Utils

mm1Rule :: String
mm1Rule = "mm1-models"

mm1Rules :: Rules ()
mm1Rules = do
    mm1Rule ~> need
        [ ruleForStabilityTraces
        , ruleForMaximumThroughputs
        ]

    subRules
        mm1RulesFor
        ruleForStabilityTraces
        allStabilityTraceExperiments

    subRules
        mm1RulesFor
        ruleForMaximumThroughputs
        allMaximumThroughputExperiments

ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-mm1-models"

ruleForMaximumThroughputs :: String
ruleForMaximumThroughputs = "maximum-throughput-mm1-models"

useMM1ModelInReport :: ExperimentConfig a => a -> Int -> Rules ()
useMM1ModelInReport ecf i = mm1ModelFileForReport ecf i `byCopying` mm1ModelTexFile ecf

dependOnMM1ModelForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnMM1ModelForReport ecf i = need [mm1ModelFileForReport ecf i]

mm1RuleFor :: ExperimentConfig a => a -> String
mm1RuleFor ecf = experimentTarget ecf ++ "-mm1-model"

mm1RulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
mm1RulesFor ecf = onlyIfResultsExist ecf $ do
    middleRule <- mm1MiddlewareRulesFor ecf
    clientsRule <- mm1ClientsRulesFor ecf
    reportRule <- mm1ReportRulesFor ecf

    let mm1target = mm1RuleFor ecf
    mm1target ~> need [middleRule, clientsRule, reportRule]
    return mm1target

readMM1ModelFile :: MonadIO m => FilePath -> m MM1Model
readMM1ModelFile = readJSON

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

mm1ReportRuleFor :: ExperimentConfig a => a -> String
mm1ReportRuleFor ecf = experimentTarget ecf ++ "-mm1-report-files"

mm1ReportRulesFor :: ExperimentConfig a => a -> Rules String
mm1ReportRulesFor ecf = do
    let modelTexFile = mm1ModelTexFile ecf
    modelTexFile %> \_ -> do
        content <- makeMM1ReportContent ecf
        writeFile' modelTexFile content

    let target_ = mm1ReportRuleFor ecf
    target_ ~> need [modelTexFile]
    pure target_

makeMM1ReportContent :: ExperimentConfig a => a -> Action String
makeMM1ReportContent ecf = do
    slocs <- readResultsSummaryLocationsForCfg ecf
    let mm1ModelFiles = map (mm1ClientsModelFileFor ecf) slocs
    mm1Models <- mapM readMM1ModelFile mm1ModelFiles
    pure $ unlines $ flip map mm1Models $ \mm1 -> tabularWithHeader
        [ "Measure", "Model"]
        [ [ "Arrival rate (transactions / second)", showDub $ avg $ arrivalRate mm1]
        , [ "Service rate (transactions / second)", showDub $ avg $ serviceRate mm1]
        , [ "Traffic intensity (no unit)", showDub $ mm1TrafficIntensity mm1]
        , [ "Mean response time (microseconds)", showDub $ time $ mm1MeanResponseTime mm1]
        , [ "Std Dev response time (microseconds)", showDub $ time $ mm1StdDevResponseTime mm1]
        , [ "Mean waiting time (microseconds)", showDub $ time $ mm1MeanWaitingTime mm1]
        , [ "Std Dev waiting time (microseconds)", showDub $ time $ mm1StdDevWaitingTime mm1]
        ]
  where
    time :: Double -> Double
    time = (* (1000 * 1000))
    showDub :: Double -> String
    showDub = printf "%.2f"

