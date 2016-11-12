{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Script
    ( traceSlicePlotsForSingleExperiment
    , traceSliceAnalysisOf
    ) where

import           Development.Shake                  hiding (doesFileExist)
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Constants
import           AslBuild.Experiment

traceSliceAnalysisScript :: FilePath
traceSliceAnalysisScript = analysisDir </> "analyze_trace_slice.r"

traceSlicePlotsForSingleExperiment :: TraceSliceAnalysisCfg -> ExperimentResultSummary -> [FilePath]
traceSlicePlotsForSingleExperiment tsa ers = do
    postfix <- ["absolute", "relative"]
    traceSlicePlotsWithPrefix $ traceSlicePlotPrefix tsa ers postfix

traceSlicePlotPrefix :: TraceSliceAnalysisCfg -> ExperimentResultSummary -> FilePath -> FilePath
traceSlicePlotPrefix TraceSliceAnalysisCfg{..} ExperimentResultSummary{..} postfix
    = analysisOutDir </> dropExtensions (takeFileName erMiddleResultsFile) ++ "-" ++ postfix

traceSlicePlotsWithPrefix :: FilePath -> [FilePath]
traceSlicePlotsWithPrefix prefix = [dropExtensions prefix ++ "-slice" <.> pngExt]

traceSliceAnalysisOf :: TraceSliceAnalysisCfg -> ExperimentResultSummary -> FilePath -> FilePath -> Rules [FilePath]
traceSliceAnalysisOf tsa ers inFile postfix = do
    let prefix = traceSlicePlotPrefix tsa ers postfix
    let plots = traceSlicePlotsWithPrefix prefix

    plots &%> \_ -> do
        need [commonRLib, traceSliceAnalysisScript, inFile]
        need [rBin]
        needRLibs ["ggplot2"]
        rScript traceSliceAnalysisScript commonRLib inFile prefix

    return plots
