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
traceSlicePlotsForSingleExperiment tsa ers = traceSlicePlotsWithPrefix $ traceSlicePlotPrefix tsa ers

traceSlicePlotPrefix :: TraceSliceAnalysisCfg -> ExperimentResultSummary -> FilePath
traceSlicePlotPrefix TraceSliceAnalysisCfg{..} ExperimentResultSummary{..}
    = analysisOutDir </> dropExtensions (takeFileName erMiddleResultsFile)

traceSlicePlotsWithPrefix :: FilePath -> [FilePath]
traceSlicePlotsWithPrefix prefix = [dropExtensions prefix ++ "-slice" <.> pngExt]

traceSliceAnalysisOf :: TraceSliceAnalysisCfg -> ExperimentResultSummary -> FilePath -> Rules [FilePath]
traceSliceAnalysisOf tsa ers inFile = do
    let prefix = traceSlicePlotPrefix tsa ers
    let plots = traceSlicePlotsForSingleExperiment tsa ers

    plots &%> \_ -> do
        need [commonRLib, traceSliceAnalysisScript, inFile]
        need [rBin]
        needRLibs ["ggplot2"]
        rScript traceSliceAnalysisScript commonRLib inFile prefix

    return plots
