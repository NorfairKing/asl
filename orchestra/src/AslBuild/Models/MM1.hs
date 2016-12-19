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
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Reports.Utils
import           AslBuild.Utils

import           AslBuild.Models.MM1.Middleware
import           AslBuild.Models.MM1.Types
import           AslBuild.Models.MM1.Utils
import           AslBuild.Models.Utils

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

mm1ModelTexFilePrefix :: ExperimentConfig a => a -> FilePath
mm1ModelTexFilePrefix ecf = reportsTmpDir </> experimentTarget ecf ++ "-mm1" <.> texExt

mm1ModelTexFileWithPostfix :: ExperimentConfig a => a -> String -> FilePath
mm1ModelTexFileWithPostfix ecf postfix = changeFilename (++ "-" ++ postfix) $ mm1ModelTexFilePrefix ecf

mm1ModelTexFiles :: ExperimentConfig a => a -> [FilePath]
mm1ModelTexFiles ecf = do
    postfix <- ["model", "measurements", "resprat", "waitrat", "arrival-rate", "service-rate"]
    pure $ mm1ModelTexFileWithPostfix ecf postfix

mm1ModelFileForReport :: FilePath -> Int -> FilePath
mm1ModelFileForReport file i = file `replaceDirectory` modelDirForReport i

useMM1ModelInReport :: ExperimentConfig a => a -> Int -> Rules ()
useMM1ModelInReport ecf i = forM_ (mm1ModelTexFiles ecf) $ \eff ->
    mm1ModelFileForReport eff i `byCopying` eff

dependOnMM1ModelForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnMM1ModelForReport ecf i = need $ map (`mm1ModelFileForReport` i) $ mm1ModelTexFiles ecf

mm1RuleFor :: ExperimentConfig a => a -> String
mm1RuleFor ecf = experimentTarget ecf ++ "-mm1-model"

mm1RulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
mm1RulesFor ecf = onlyIfResultsExist ecf $ do
    estimateRule <- mm1EstimationRulesFor ecf
    middleRule <- mm1MiddlewareRulesFor ecf
    reportR <- mm1ReportRulesFor ecf

    let mm1target = mm1RuleFor ecf
    mm1target ~> need [estimateRule, middleRule, reportR]
    return mm1target

readMM1ModelFile :: MonadIO m => FilePath -> m MM1Model
readMM1ModelFile = readJSON

mm1EstimationRuleFor :: ExperimentConfig a => a -> String
mm1EstimationRuleFor ecf = experimentTarget ecf ++ "-mm1-estimation"

mm1ModelEstimateFileFor :: ExperimentConfig a => a -> [FilePath] -> FilePath
mm1ModelEstimateFileFor ecf = changeFilename (const "mm1-estimate") . (`replaceSndDir` experimentAnalysisTmpDir ecf) . head

mm1EstimationRulesFor :: ExperimentConfig a => a -> Rules String
mm1EstimationRulesFor ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    mm1ModelFiles <- forM slocss $ \slocs -> do
        let modelFile = mm1ModelEstimateFileFor ecf slocs
        modelFile %> \outf -> do
            mm1Model <- estimateMM1Model ecf slocs
            writeJSON outf mm1Model
        return modelFile

    let mm1target = mm1EstimationRuleFor ecf
    mm1target ~> need mm1ModelFiles
    return mm1target

estimateMM1Model :: ExperimentConfig a => a -> [FilePath] -> Action MM1Model
estimateMM1Model ecf slocs = do
    let combinedResultsFile = combinedClientRepsetResultsFile ecf slocs
    need [combinedResultsFile]
    res <- readCombinedClientsResults combinedResultsFile
    -- Arrival rate as Average throughput
    let λ = avgAvgs $ avgBothResults $ avgTpsResults res
    -- Service rate as Maxiumum throughput
    let μ = avg $ avgMaxTps res
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
    let modelTexFiles = mm1ModelTexFiles ecf
    modelTexFiles &%> \_ -> makeMM1ReportContent ecf

    let target_ = mm1ReportRuleFor ecf
    target_ ~> need modelTexFiles
    pure target_

makeMM1ReportContent :: ExperimentConfig a => a -> Action ()
makeMM1ReportContent ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    forP_ slocss $ \slocs -> do
        erss <- mapM readResultsSummary slocs
        mrfs <- case mapM merMiddleResultsFile erss of
            Nothing -> fail "must have a middleware to evaluate mm1 model."
            Just m  -> pure m

        let combAvgDurFile = combinedAvgDurationFile ecf mrfs
        let mm1ModelFile = mm1ModelEstimateFileFor ecf slocs
        let combinedResultsFile = combinedClientRepsetResultsFile ecf slocs
        setup <- readExperimentSetupForSummary $ head erss
        need [combAvgDurFile, mm1ModelFile, combinedResultsFile]
        mm1 <- readMM1ModelFile mm1ModelFile
        combAvgDurs <- readCombinedAvgDursFile combAvgDurFile

        let metaResp = totalDuration combAvgDurs
        let actualMeanResp = avgAvgs metaResp
        let actualStdDevResp = combStdDev metaResp
        let metaWait = untilDequeuedTime combAvgDurs
        let actualAvgWait = avgAvgs metaWait
        let actualStdDevWait = combStdDev metaWait
        let totalNrClients = nrUsers setup

        mf <- case backendSetup setup of
                Left _        -> fail "need middleware."
                Right (ms, _) -> pure $ mMiddlewareFlags ms
        let readThreadsPerServer = mwNrThreads mf
            nrSers = length $ mwServers mf
        let totalNrWorkers = (readThreadsPerServer + 1) * nrSers

        let predictedMeanResp = mm1MeanResponseTime mm1
        let predictedMeanWait = mm1MeanWaitingTime mm1

        writeFile' (mm1ModelTexFileWithPostfix ecf "arrival-rate") (showDub $ arrivalRate mm1)
        writeFile' (mm1ModelTexFileWithPostfix ecf "service-rate") (showDub $ serviceRate mm1)
        writeFile' (mm1ModelTexFileWithPostfix ecf "resprat") (showDub $ timeFromMiddle actualMeanResp / timeFromModel predictedMeanWait)
        writeFile' (mm1ModelTexFileWithPostfix ecf "waitrat") (showDub $ timeFromMiddle actualAvgWait / timeFromModel predictedMeanWait)

        let t1 = tabularWithHeader
                [ "Measure", "Model", "Unit"]
                [ dline "Arrival rate"                  (arrivalRate mm1) "(transactions / second)"
                , dline "Service rate"                  (serviceRate mm1) "(transactions / second)"
                , dline "Traffic intensity"             (mm1TrafficIntensity mm1) ""
                , dline "Mean response time"            predictedMeanResp "$\\mu s$"
                , dline "Std Dev response time"         (timeFromModel $ mm1StdDevResponseTime mm1) "$\\mu s$"
                , dline "Mean waiting time"             predictedMeanWait "$\\mu s$"
                , dline "Std Dev waiting time"          (timeFromModel $ mm1StdDevWaitingTime mm1) "$\\mu s$"
                , dline "Jobs in the queue"             (mm1WaitingJobs mm1) "Jobs"
                , dline "Jobs in the system"            (mm1MeanNrJobs mm1) "Jobs"
                , dline "Jobs in queue/jobs in system"  (mm1WaitingJobs mm1 / mm1MeanNrJobs mm1) ""
                ]
        writeFile' (mm1ModelTexFileWithPostfix ecf "model") t1

        let t2 = tabularWithHeader
                [ "Measure", "Measurement", "Unit"]
                [ dline "Mean time in middleware"       (timeFromMiddle actualMeanResp) "$\\mu s$"
                , dline "Std Dev time in middleware"    (timeFromMiddle actualStdDevResp) "$\\mu s$"
                , dline "Mean time in queue"            (timeFromMiddle actualAvgWait) "$\\mu s$"
                , dline "Std Dev time in queue"         (timeFromMiddle actualStdDevWait) "$\\mu s$"
                , iline "Jobs in the system"            totalNrClients "Jobs"
                , ["Total nr of workers", unwords ["$(", show readThreadsPerServer, "+ 1)", "\\times", show nrSers, "=", show totalNrWorkers, "$"], "Jobs"]
                , ["Jobs in the queue", unwords["$(", show totalNrClients, "-", show totalNrWorkers, ") =", show $ totalNrClients - totalNrWorkers, "$"], "Jobs"]
                , dline "Jobs in queue/jobs in system"  (fromIntegral (totalNrClients - totalNrWorkers) / fromIntegral totalNrClients ) ""
                ]
        writeFile' (mm1ModelTexFileWithPostfix ecf "measurements") t2


  where
    -- Model only
    dline :: String -> Double -> String -> [String]
    dline title val u = [title, showDub val, u]
    iline :: Show i => String -> i -> String -> [String]
    iline title val u = [title, show val, u]
    timeFromModel :: Double -> Double
    timeFromModel = (* (1000 * 1000))
    timeFromMiddle :: Double -> Double
    timeFromMiddle = (/ 1000)
    showDub :: Double -> String
    showDub = printf "$%.3f$"

