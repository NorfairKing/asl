{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Script
    ( traceSlicePlotsForSingleExperiment
    , traceSliceAnalysisOf
    ) where

import           Development.Shake          hiding (doesFileExist)
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment

traceSliceAnalysisScript :: FilePath
traceSliceAnalysisScript = analysisDir </> "analyze_trace_slice.r"

traceSlicePlotsForSingleExperiment :: ExperimentConfig a => a ->ExperimentResultSummary -> [FilePath]
traceSlicePlotsForSingleExperiment ecf ers = do
    postfix <- ["absolute", "relative"]
    traceSlicePlotsWithPrefix $ traceSlicePlotPrefix ecf ers postfix

traceSlicePlotPrefix :: ExperimentConfig a => a -> ExperimentResultSummary -> FilePath -> FilePath
traceSlicePlotPrefix ecf ExperimentResultSummary{..} postfix
    = experimentPlotsDir ecf </> dropExtensions (takeFileName erMiddleResultsFile) ++ "-" ++ postfix

traceSlicePlotsWithPrefix :: FilePath -> [FilePath]
traceSlicePlotsWithPrefix prefix = [dropExtensions prefix ++ "-slice" <.> pngExt]

traceSliceAnalysisOf :: ExperimentConfig a => a -> ExperimentResultSummary -> FilePath -> FilePath -> Rules [FilePath]
traceSliceAnalysisOf ecf ers inFile postfix = do
    let prefix = traceSlicePlotPrefix ecf ers postfix
    let plots = traceSlicePlotsWithPrefix prefix

    plots &%> \_ -> do
        need [commonRLib, traceSliceAnalysisScript, inFile]
        need [rBin]
        needRLibs ["ggplot2"]
        rScript traceSliceAnalysisScript commonRLib inFile prefix

    return plots
