module AslBuild.Models.MMm where

import Control.Monad

import           Development.Shake

import           AslBuild.Analysis.Utils
import           AslBuild.Utils
import AslBuild.Models.MMm.Types
import AslBuild.Models.MMm.Report
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
    estt <- mmmEstimationRulesFor ecf
    repf <- mmmReportRulesFor ecf

    let mmmtarget = mmmRuleFor ecf
    mmmtarget ~> need [estt, repf]
    return mmmtarget

mmmEstimationRuleFor :: ExperimentConfig a => a -> String
mmmEstimationRuleFor ecf = experimentTarget ecf ++ "-mmm-estimation"

mmmModelEstimateFileFor :: ExperimentConfig a => a -> [FilePath] -> FilePath
mmmModelEstimateFileFor ecf = changeFilename (const "mmm-estimate") . (`replaceSndDir` experimentAnalysisTmpDir ecf) . head

mmmEstimationRulesFor :: ExperimentConfig a => a -> Rules String
mmmEstimationRulesFor ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    mmmModelFiles <- forM slocss $ \slocs -> do
        let modelFile = mmmModelEstimateFileFor ecf slocs
        modelFile %> \_ -> do
            mmmModel <- estimateMMmModel ecf slocs
            writeJSON modelFile mmmModel
        pure modelFile

    let rule = mmmEstimationRuleFor ecf
    rule ~> need mmmModelFiles
    pure rule

estimateMMmModel :: ExperimentConfig a => a -> [FilePath] -> Action MMmModel
estimateMMmModel ecf slocs = do
    let λ = 0
        μ = 0
        m = 0
    pure $ MMmModel λ μ m

useMMmModelInReport :: ExperimentConfig a => a -> Int -> Rules ()
useMMmModelInReport ecf i = mmmModelFileForReport ecf i `byCopying` mmmModelTexFile ecf

dependOnMMmModelForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnMMmModelForReport ecf i = need [mmmModelFileForReport ecf i]

mmmReportRuleFor :: ExperimentConfig a => a -> String
mmmReportRuleFor ecf = experimentTarget ecf ++ "-mmm-report-files"

mmmReportRulesFor :: ExperimentConfig a => a -> Rules String
mmmReportRulesFor ecf = do
    let modelTexFile = mmmModelTexFile ecf
    modelTexFile %> \_ -> do
        content <- makeMMmReportContent ecf
        writeFile' modelTexFile content

    let rule = mmmReportRuleFor ecf
    rule ~> need [modelTexFile]
    pure rule


makeMMmReportContent :: ExperimentConfig a => a -> Action String
makeMMmReportContent ecf =
    pure "hi"
