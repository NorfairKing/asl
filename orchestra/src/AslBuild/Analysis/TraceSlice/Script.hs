{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AslBuild.Analysis.TraceSlice.Script
    ( traceSlicePlotsForSingleExperiment
    , traceSliceAnalysisOf
    ) where

import Data.List

import Development.Shake hiding (doesFileExist)
import Development.Shake.FilePath

import AslBuild.Analysis.BuildR
import AslBuild.Analysis.Common
import AslBuild.Analysis.Utils
import AslBuild.Constants
import AslBuild.Experiment
import AslBuild.Experiments.MaximumThroughput

traceSliceAnalysisScript :: FilePath
traceSliceAnalysisScript = analysisDir </> "analyze_trace_slice.r"

traceSlicePlotsForSingleExperiment :: MaximumThroughputCfg -> [FilePath]
traceSlicePlotsForSingleExperiment mtc = do
    postfix <- ["absolute", "relative"]
    traceSlicePlotsWithPrefix mtc $ traceSlicePlotPrefix mtc postfix

traceSlicePlotPrefix :: MaximumThroughputCfg -> FilePath -> FilePath
traceSlicePlotPrefix ecf postfix =
    experimentPlotsDir ecf </> experimentTarget ecf ++ "-slice-" ++ postfix

traceSlicePlotsWithPrefix :: MaximumThroughputCfg -> FilePath -> [FilePath]
traceSlicePlotsWithPrefix MaximumThroughputCfg {..} prefix = do
    middleThreads <- nub $ map fst threadConcTups
    return $
        dropExtensions prefix ++ "-" ++ intercalate "-" [show middleThreads, "slice"] <.> pngExt

traceSliceAnalysisOf :: MaximumThroughputCfg -> FilePath -> FilePath -> Rules [FilePath]
traceSliceAnalysisOf ecf inFile postfix = do
    let prefix = traceSlicePlotPrefix ecf postfix
    let plots = traceSlicePlotsWithPrefix ecf prefix
    plots &%> \_ -> do
        need [commonRLib, traceSliceAnalysisScript, inFile]
        need [rBin]
        needRLibs ["ggplot2"]
        rScript traceSliceAnalysisScript commonRLib inFile prefix postfix
    return plots
