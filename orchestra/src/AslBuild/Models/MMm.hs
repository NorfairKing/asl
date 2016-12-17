module AslBuild.Models.MMm where

import           Control.Monad

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Models.MMm.Internal
import           AslBuild.Models.MMm.Report
import           AslBuild.Models.MMm.Types
import           AslBuild.Reports.Common
import           AslBuild.Utils

mmmRule :: String
mmmRule = "mmm-models"

mmmRules :: Rules ()
mmmRules = do
    mmmRule ~> need [ruleForReplicationEffects]

    subRules
        mmmRulesFor
        ruleForReplicationEffects
        allReplicationEffectExperiments

ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-mmm-models"

mmmRuleFor :: ExperimentConfig a => a -> String
mmmRuleFor ecf = experimentTarget ecf ++ "-mmm-models"

mmmRulesFor :: ReplicationEffectCfg -> Rules (Maybe String)
mmmRulesFor ecf = onlyIfResultsExist ecf $ do
    estt <- mmmEstimationRulesFor ecf
    repf <- mmmReportRulesFor ecf
    plotfs <- mmmPlotsRulesFor ecf

    let mmmtarget = mmmRuleFor ecf
    mmmtarget ~> need [estt, repf, plotfs]
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
            mmm <- estimateMMmModel ecf slocs
            writeJSON modelFile mmm
        pure modelFile

    let rule = mmmEstimationRuleFor ecf
    rule ~> need mmmModelFiles
    pure rule

estimateMMmModel :: ExperimentConfig a => a -> [FilePath] -> Action MMmModel
estimateMMmModel ecf slocs = do
    let combinedResultsFile = combinedClientRepsetResultsFile ecf slocs
    need [combinedResultsFile]
    res <- readCombinedClientsResults combinedResultsFile
    ers <- readResultsSummary $ head slocs
    setup <- readExperimentSetupForSummary ers

    m <- case backendSetup setup of
        Left _        -> fail "must have middleware to compute mmm model"
        Right (_, ss) -> pure $ length ss

    -- Arrival rate as Average throughput
    let λ = avgAvgs $ avgBothResults $ avgTpsResults res
    -- Service rate as Maxiumum throughput divided by the number of servers
    let μ = avg (avgMaxTps res) / fromIntegral m
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
    pure $ experimentTarget ecf


mmmReplicationEffectPlotsFor :: ReplicationEffectCfg -> [FilePath]
mmmReplicationEffectPlotsFor ecf = [experimentPlotsDir ecf </> experimentTarget ecf ++ "-mmm-model" <.> pngExt]

useMMmPlotsInReport :: ReplicationEffectCfg -> Int -> Rules ()
useMMmPlotsInReport ecf = usePlotsInReport $ mmmReplicationEffectPlotsFor ecf

dependOnMMmPlotsForReport :: ReplicationEffectCfg -> Int -> Action ()
dependOnMMmPlotsForReport ecf = dependOnPlotsForReport $ mmmReplicationEffectPlotsFor ecf

mmmSimplifiedReplicationCsv :: ReplicationEffectCfg -> FilePath
mmmSimplifiedReplicationCsv ecf = experimentAnalysisTmpDir ecf </> experimentTarget ecf ++ "-simplified-mmm" <.> csvExt

mmmReplicationAnalysisScript :: FilePath
mmmReplicationAnalysisScript = analysisDir </> "mmm_analysis.r"

mmmPlotsRuleFor :: ExperimentConfig a => a -> String
mmmPlotsRuleFor ecf = experimentTarget ecf ++ "-mmm-plot-files"

mmmPlotsRulesFor :: ReplicationEffectCfg -> Rules String
mmmPlotsRulesFor ecf = do
    let csvFile = mmmSimplifiedReplicationCsv ecf
    csvFile %> \_ -> do
        slocss <- readResultsSummaryLocationsForCfg ecf
        ls <- forM slocss $ \slocs -> do
            let mmmFile = mmmModelEstimateFileFor ecf slocs
            let combinedResultsFile = combinedClientRepsetResultsFile ecf slocs
            need [mmmFile, combinedResultsFile]
            mmm <- readJSON mmmFile
            crs <- readCombinedClientsResults combinedResultsFile
            pure SimplifiedReplicationCsvLine
                { mmmModel = mmm
                , actualTps = avgBothResults $ avgTpsResults crs
                , actualResp = avgBothResults $ avgRespResults crs
                }
        putLoud $ unwords ["Making simplified CSV file", csvFile, "for" ++ experimentTarget ecf]
        writeCSV csvFile ls

    let plots = mmmReplicationEffectPlotsFor ecf
    plots &%> \_ -> do
        need [csvFile, mmmReplicationAnalysisScript, commonRLib, rBin, csvFile]
        putLoud $ unwords ["Making plots from CSV file", csvFile, ": ", show plots]
        rScript mmmReplicationAnalysisScript commonRLib csvFile $ dropExtensions $ head plots


    let rule = mmmPlotsRuleFor ecf
    rule ~> need (csvFile : plots)
    pure rule



