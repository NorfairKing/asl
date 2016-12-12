module AslBuild.Models.MM1 where

import           Control.Monad
import           Control.Monad.IO.Class
import           Text.Printf

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Reports.Utils
import           AslBuild.Utils

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
    estimateRule <- mm1EstimationRulesFor ecf
    middleRule <- mm1MiddlewareRulesFor ecf
    reportRule <- mm1ReportRulesFor ecf

    let mm1target = mm1RuleFor ecf
    mm1target ~> need [estimateRule, middleRule, reportRule]
    return mm1target

readMM1ModelFile :: MonadIO m => FilePath -> m MM1Model
readMM1ModelFile = readJSON

mm1EstimationRuleFor :: ExperimentConfig a => a -> String
mm1EstimationRuleFor ecf = experimentTarget ecf ++ "-middleware-mm1-estimation"

mm1ModelEstimateFileFor :: ExperimentConfig a => a -> FilePath -> FilePath
mm1ModelEstimateFileFor ecf = changeFilename (++ "-mm1-estimate") . (`replaceDirectory` experimentAnalysisTmpDir ecf)

mm1EstimationRulesFor :: ExperimentConfig a => a -> Rules String
mm1EstimationRulesFor ecf = do
    -- TODO combine the repititions first?
    slocs <- readResultsSummaryLocationsForCfg ecf
    mm1ModelFiles <- forM (concat slocs) $ \sloc -> do
        let modelFile = mm1ModelEstimateFileFor ecf sloc
        modelFile %> \outf -> do
            mm1Model <- estimateMM1Model ecf sloc
            writeJSON outf mm1Model
        return modelFile

    let mm1target = mm1EstimationRuleFor ecf
    mm1target ~> need mm1ModelFiles
    return mm1target

estimateMM1Model :: ExperimentConfig a => a -> FilePath -> Action MM1Model
estimateMM1Model ecf sloc = do
    let combinedResultsFile = combineClientResultsFile ecf sloc
    need [combinedResultsFile]
    res <- readCombinedClientResults combinedResultsFile
    ers <- readResultsSummary sloc
    setup <- readExperimentSetupForSummary ers
    let mtps = avg $ bothResults $ tpsResults res
        -- mrst = avg $ bothResults $ respResults res
        -- Number of clients
    let nrc = fromIntegral $ nrUsers setup
    let λ = mtps
    let μ = ((1 + nrc) * λ) / nrc
    pure $ MM1Model λ μ



mm1MiddlewareRuleFor :: ExperimentConfig a => a -> String
mm1MiddlewareRuleFor ecf = experimentTarget ecf ++ "-middleware-mm1-model"

mm1MiddlewareRulesFor :: ExperimentConfig a => a -> Rules String
mm1MiddlewareRulesFor ecf = do
    -- TODO combine repititions
    slocs <- readResultsSummaryLocationsForCfg ecf
    mm1ModelFiles <- forM (concat slocs) $ \sloc -> do
        let modelFile = mm1MiddlewareModelFileFor ecf sloc
        modelFile %> \outf -> do
            mm1Model <- calcMiddlewareMM1Model sloc
            writeJSON outf mm1Model
        return modelFile

    let mm1target = mm1MiddlewareRuleFor ecf
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
    -- TODO combine repititions
    slocs <- readResultsSummaryLocationsForCfg ecf
    (unlines <$>) $ forP (concat slocs) $ \sloc -> do
        ers <- readResultsSummary sloc
        mrf <- case merMiddleResultsFile ers of
            Nothing -> fail "must have a middleware to evaluate mm1 model."
            Just m -> pure m

        let avgDurFile = avgDurationFile ecf mrf
        let mm1ModelFile = mm1ModelEstimateFileFor ecf sloc
        let mm1MModelFile = mm1MiddlewareModelFileFor ecf sloc
        let combinedResultsFile = combineClientResultsFile ecf sloc
        need [avgDurFile, mm1ModelFile, mm1MModelFile, combinedResultsFile]
        mm1 <- readMM1ModelFile mm1ModelFile
        res <- readCombinedClientResults combinedResultsFile
        avgDurs <- readJSON avgDurFile :: Action (Durations Avg)

        pure $ tabularWithHeader
            [ "Measure", "Model", "Measurement", "Relative difference"]
            [ mo "Arrival rate (transactions / second)"  (arrivalRate mm1)
            , mo "Service rate (transactions / second)"  (serviceRate mm1)
            , mo "Traffic intensity (no unit)"           (mm1TrafficIntensity mm1)
            , line "Mean response time ($\\mu s$)"         (timeFromModel $ mm1MeanResponseTime mm1)    (avg $ bothResults $ respResults res)
            , line "Std Dev response time ($\\mu s$)"      (timeFromModel $ mm1StdDevResponseTime mm1)  (stdDev $ bothResults $ respResults res)
            , line "Mean waiting time ($\\mu s$)"          (timeFromModel $ mm1MeanWaitingTime mm1)     (timeFromMiddle $ avg $ untilDequeuedTime avgDurs)
            , line "Std Dev waiting time ($\\mu s$)"       (timeFromModel $ mm1StdDevWaitingTime mm1)   (timeFromMiddle $ stdDev $ untilDequeuedTime avgDurs)
            ]
  where
    -- Model only
    mo :: String -> Double -> [String]
    mo title model = [title, showDub model, "", ""]
    line :: String -> Double -> Double -> [String]
    line title model real = [title, showDub model, showDub real, showDub ((real / model) - 1)]
    timeFromModel :: Double -> Double
    timeFromModel = (* (1000 * 1000))
    timeFromMiddle :: Double -> Double
    timeFromMiddle = (/ 1000)
    showDub :: Double -> String
    showDub = printf "$%.3f$"

