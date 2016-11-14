{-# LANGUAGE OverloadedStrings #-}
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

traceSlicePlotsForSingleExperiment :: ExperimentConfig a => a -> [FilePath]
traceSlicePlotsForSingleExperiment ecf = do
    postfix <- ["absolute", "relative"]
    traceSlicePlotsWithPrefix $ traceSlicePlotPrefix ecf postfix

traceSlicePlotPrefix :: ExperimentConfig a => a -> FilePath -> FilePath
traceSlicePlotPrefix ecf postfix
    = experimentPlotsDir ecf </> dropExtensions (takeFileName $ resultSummariesLocationFile ecf) ++ "-" ++ postfix

traceSlicePlotsWithPrefix :: FilePath -> [FilePath]
traceSlicePlotsWithPrefix prefix = [dropExtensions prefix ++ "-slice" <.> pngExt]

traceSliceAnalysisOf :: ExperimentConfig a => a -> FilePath -> FilePath -> Rules [FilePath]
traceSliceAnalysisOf ecf inFile postfix = do
    let prefix = traceSlicePlotPrefix ecf postfix
    let plots = traceSlicePlotsWithPrefix prefix

    plots &%> \_ -> do
        need [commonRLib, traceSliceAnalysisScript, inFile]
        need [rBin]
        needRLibs ["ggplot2"]
        rScript traceSliceAnalysisScript commonRLib inFile prefix postfix

    return plots
