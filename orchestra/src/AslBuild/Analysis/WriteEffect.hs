{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.WriteEffect where

import           Data.List

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Analysis.WriteEffect.Types
import           AslBuild.Client.Types
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Memaslap.Types
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Reports.Common
import           AslBuild.Utils

writeAnalysisRule :: String
writeAnalysisRule = "write-analysis"

writeAnalysisRules :: Rules ()
writeAnalysisRules = do
    writeAnalysisRule ~> need
        [ ruleForWriteEffects
        ]

    subRules
        rulesForWriteAnalysis
        ruleForWriteEffects
        allWriteEffectExperiments

ruleForWriteEffects :: String
ruleForWriteEffects = "write-effect-write-analysis"

ruleForWriteAnalysis :: ExperimentConfig a => a -> String
ruleForWriteAnalysis rec
    = experimentTarget rec ++ "-write-analysis"

writeAnalysisPlotsFor :: WriteEffectCfg -> [FilePath]
writeAnalysisPlotsFor rec = do
    nrSers <- serverCounts rec
    metric <- ["tps", "resp"]
    return $ intercalate "-" [writeAnalysisPrefixFor rec, metric, show nrSers] <.> pngExt

simplifiedWriteCsv :: WriteEffectCfg -> FilePath
simplifiedWriteCsv rec
    = experimentAnalysisTmpDir rec </> experimentTarget rec ++ "-simplified-write-analysis" <.> csvExt

writeAnalysisPrefixFor :: WriteEffectCfg -> String
writeAnalysisPrefixFor rec
    = experimentPlotsDir rec </> experimentTarget rec ++ "-write-analysis"

useWriteEffectPlotsInReport :: WriteEffectCfg -> Int -> Rules ()
useWriteEffectPlotsInReport rec
    = usePlotsInReport $ writeAnalysisPlotsFor rec

dependOnWriteEffectPlotsForReport :: WriteEffectCfg -> Int -> Action ()
dependOnWriteEffectPlotsForReport rec
    = dependOnPlotsForReport $ writeAnalysisPlotsFor rec

writeAnalysisScript :: FilePath
writeAnalysisScript = analysisDir </> "analyze_write_effect.r"

rulesForWriteAnalysis :: WriteEffectCfg -> Rules (Maybe String)
rulesForWriteAnalysis rec = onlyIfResultsExist rec $ do
    let plots1 = writeAnalysisPlotsFor rec

    let simplifiedCsv = simplifiedWriteCsv rec
    simplifiedCsv %> \outFile -> do
        -- TODO combine repititions
        slocs <- readResultsSummaryLocationsForCfg rec
        let combinedResultsFiles = map (combineClientResultsFile rec) $ concat slocs
        need combinedResultsFiles
        lines_ <- forP (concat slocs) $ \sloc -> do
            ers <- readResultsSummary sloc
            setup <- readExperimentSetupForSummary ers
            res <- readCombinedClientResults $ combineClientResultsFile rec sloc
            return $ simplifiedCsvLines setup res

        writeCSV outFile lines_

    plots1 &%> \_ -> do
        need [writeAnalysisScript, commonRLib, rBin, simplifiedCsv]
        needRLibs ["ggplot2"]
        rScript writeAnalysisScript commonRLib simplifiedCsv $ writeAnalysisPrefixFor rec


    let analysisTarget = ruleForWriteAnalysis rec
    analysisTarget ~> need plots1
    return analysisTarget


simplifiedCsvLines :: ExperimentSetup -> MemaslapClientResults -> SimplifiedCsvLine
simplifiedCsvLines ExperimentSetup{..} MemaslapClientResults{..} =
    let respA = bothResults respResults
        tpsA = bothResults tpsResults
        (ms, sss) = fromRight backendSetup
        cs = clientSetups
    in SimplifiedCsvLine
        { nrServers = length sss
        , writePercentage = maximum $ map (setProportion . msConfig . cMemaslapSettings) cs
        , replicationFactor = mwReplicationFactor $ mMiddlewareFlags ms
        , respAvg = respA
        , tpsAvg = tpsA
        }
