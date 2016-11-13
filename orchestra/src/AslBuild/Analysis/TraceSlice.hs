{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice
    ( module AslBuild.Analysis.TraceSlice
    , module AslBuild.Analysis.TraceSlice.Types
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.IO

import           Development.Shake
import           Development.Shake.FilePath

import           Pipes                                  ((>->))

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Script
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.Baseline
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Reports.Common

traceSliceAnalysisRule :: String
traceSliceAnalysisRule = "trace-slice-analysis"

traceSliceAnalysisRules :: Rules ()
traceSliceAnalysisRules = do
    traceSliceAnalysisRule ~> need
        [ ruleForBaselines
        , ruleForStabilityTraces
        , ruleForMaximumThroughputs
        , ruleForWriteEffects
        , ruleForReplicationEffects
        ]

    let traceSliceSubrules :: ExperimentConfig a => String -> [a] -> Rules ()
        traceSliceSubrules = subRules rulesForTraceSliceAnalysis

    traceSliceSubrules ruleForBaselines allBaselineExperiments
    traceSliceSubrules ruleForStabilityTraces allStabilityTraceExperiments
    traceSliceSubrules ruleForMaximumThroughputs allMaximumThroughputExperiments
    traceSliceSubrules ruleForWriteEffects allWriteEffectExperiments
    traceSliceSubrules ruleForReplicationEffects allReplicationEffectExperiments


ruleForBaselines :: String
ruleForBaselines = "baseline-trace-slice-analysis"

ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-trace-slice-analysis"

ruleForMaximumThroughputs :: String
ruleForMaximumThroughputs = "maximum-throughput-trace-slice-analysis"

ruleForWriteEffects :: String
ruleForWriteEffects = "write-effect-trace-slice-analysis"

ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-trace-slice-analysis"

experimentAnalysisCfgRule :: ExperimentConfig a => a -> String
experimentAnalysisCfgRule cfg = experimentTarget cfg ++ "-trace-slice-analysis"

useTraceSlicePlotsInReport :: ExperimentConfig a => a -> Int -> Rules ()
useTraceSlicePlotsInReport ecf i
    = traceSlicePlotsFor ecf >>= (`usePlotsInReport` i)

dependOnTraceSlicePlotsForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnTraceSlicePlotsForReport ecf i
    = traceSlicePlotsFor ecf >>= (`dependOnPlotsForReport` i)

traceSlicePlotsFor :: (MonadIO m, ExperimentConfig a) => a -> m [FilePath]
traceSlicePlotsFor ecf = do
    slocs <- readResultsSummaryLocationsForCfg ecf
    summaries <- mapM readResultsSummary slocs
    return $ concatMap (traceSlicePlotsForSingleExperiment ecf) summaries

rulesForTraceSliceAnalysis :: ExperimentConfig a => a -> Rules (Maybe String)
rulesForTraceSliceAnalysis ecf = onlyIfResultsExist ecf $ do
    summaryPaths <- readResultsSummaryLocationsForCfg ecf
    pss <- forM summaryPaths $ \summaryPath -> do
        ers@ExperimentResultSummary{..} <- readResultsSummary summaryPath
        case merMiddleResultsFile of
            Nothing -> return []
            Just erMiddleResultsFile -> do
                let dFile = durationsFile ecf erMiddleResultsFile
                    adFile = absDurationsFile ecf erMiddleResultsFile
                    rdFile = relDurationsFile ecf erMiddleResultsFile

                dFile %> \outFile -> do
                    need [erMiddleResultsFile]
                    size <- liftIO $ withFile erMiddleResultsFile ReadMode hFileSize
                    -- 90 characters in the header line, then 95 characters per line
                    let totalLines = (size - 90) `div` 95
                    let window = max 1 $ totalLines `div` 20

                    putLoud $ unwords
                        [ "Distilling trace slice data from"
                        , erMiddleResultsFile
                        , "into"
                        , outFile
                        , "with window size"
                        , show window
                        ]
                    transformCsvFileAction erMiddleResultsFile outFile $
                            timeTransformer
                        >-> meanTransformer window

                adFile %> \outFile -> do
                    need [dFile]
                    putLoud $ unwords
                        [ "Gathering absolute trace slice data from"
                        , dFile
                        , "into"
                        , outFile
                        ]
                    transformCsvFileAction dFile outFile absLineTransformer

                rdFile %> \outFile -> do
                    need [dFile]
                    putLoud $ unwords
                        [ "Gathering relative trace slice data from"
                        , dFile
                        , "into"
                        , outFile
                        ]
                    transformCsvFileAction dFile outFile relLineTransformer

                aplots <- traceSliceAnalysisOf ecf ers adFile "absolute"
                rplots <- traceSliceAnalysisOf ecf ers rdFile "relative"
                return $ aplots ++ rplots

    let analysisTarget = experimentAnalysisCfgRule ecf
    analysisTarget ~> need (concat pss)
    return analysisTarget

durationsFile :: ExperimentConfig a => a -> FilePath -> FilePath
durationsFile ecf f = experimentAnalysisTmpDir ecf </> dropExtensions (takeFileName f) ++ "-durations" <.> csvExt

absDurationsFile :: ExperimentConfig a => a -> FilePath -> FilePath
absDurationsFile ecf f = experimentAnalysisTmpDir ecf </> dropExtensions (takeFileName f) ++ "-absolute-durations" <.> csvExt

relDurationsFile :: ExperimentConfig a => a -> FilePath -> FilePath
relDurationsFile ecf f = experimentAnalysisTmpDir ecf </> dropExtensions (takeFileName f) ++ "-relative-durations" <.> csvExt
