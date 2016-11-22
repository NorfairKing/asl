{-# LANGUAGE OverloadedStrings #-}
module AslBuild.Analysis.TraceSlice
    ( module AslBuild.Analysis.TraceSlice
    , module AslBuild.Analysis.TraceSlice.Types
    ) where

import           Control.Monad.IO.Class

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Script
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Analysis.TraceSlice.Utils
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Reports.Common

traceSliceAnalysisRule :: String
traceSliceAnalysisRule = "trace-slice-analysis"

traceSliceAnalysisRules :: Rules ()
traceSliceAnalysisRules = do
    traceSliceAnalysisRule ~> need
        [ ruleForMaximumThroughputs
        ]

    subRules
        rulesForTraceSliceAnalysis
        ruleForMaximumThroughputs
        allMaximumThroughputExperiments

ruleForMaximumThroughputs :: String
ruleForMaximumThroughputs = "maximum-throughput-trace-slice-analysis"

experimentAnalysisCfgRule :: MaximumThroughputCfg -> String
experimentAnalysisCfgRule cfg = experimentTarget cfg ++ "-trace-slice-analysis"

useTraceSlicePlotsInReport :: MaximumThroughputCfg -> Int -> Rules ()
useTraceSlicePlotsInReport ecf i
    = traceSlicePlotsFor ecf >>= (`usePlotsInReport` i)

dependOnTraceSlicePlotsForReport :: MaximumThroughputCfg -> Int -> Action ()
dependOnTraceSlicePlotsForReport ecf i
    = traceSlicePlotsFor ecf >>= (`dependOnPlotsForReport` i)

traceSlicePlotsFor :: MonadIO m => MaximumThroughputCfg -> m [FilePath]
traceSlicePlotsFor = return . traceSlicePlotsForSingleExperiment

rulesForTraceSliceAnalysis :: MaximumThroughputCfg -> Rules (Maybe String)
rulesForTraceSliceAnalysis ecf = onlyIfResultsExist ecf $ do

    summaryPaths <- readResultsSummaryLocationsForCfg ecf
    ers <-  mapM readResultsSummary summaryPaths
    emrs <- case mapM merMiddleResultsFile ers of
        Nothing -> fail "should have a middleware."
        Just emrs -> return emrs

    let dFile = avgDurationsFile ecf
        adFile = absDurationsFile ecf
        rdFile = relDurationsFile ecf

    -- TODO need averages
    dFile %> \outFile -> do
        need $ map (avgDurationFile ecf) emrs
        buildAvgDursFile ecf outFile

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

    aplots <- traceSliceAnalysisOf ecf adFile "absolute"
    rplots <- traceSliceAnalysisOf ecf rdFile "relative"

    let analysisTarget = experimentAnalysisCfgRule ecf
    analysisTarget ~> need (aplots ++ rplots)
    return analysisTarget

avgDurationsFile :: MaximumThroughputCfg -> FilePath
avgDurationsFile ecf = experimentAnalysisTmpDir ecf </> experimentTarget ecf ++ "-avg-durations" <.> csvExt

absDurationsFile :: MaximumThroughputCfg -> FilePath
absDurationsFile ecf = experimentAnalysisTmpDir ecf </> experimentTarget ecf ++ "-absolute-durations" <.> csvExt

relDurationsFile :: MaximumThroughputCfg -> FilePath
relDurationsFile ecf = experimentAnalysisTmpDir ecf </> experimentTarget ecf ++ "-relative-durations" <.> csvExt
