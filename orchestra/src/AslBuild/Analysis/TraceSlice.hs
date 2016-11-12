{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice
    ( module AslBuild.Analysis.TraceSlice
    , module AslBuild.Analysis.TraceSlice.Types
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           System.IO

import           Development.Shake
import           Development.Shake.FilePath

import           Pipes                                  ((>->))

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Script
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiment
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
        [ ruleForStabilityTraces
        , ruleForMaximumThroughputs
        , ruleForWriteEffects
        , ruleForReplicationEffects
        ]

    rulesForStabilityTraces
        [ smallLocalStabilityTrace
        , localStabilityTrace
        , smallRemoteStabilityTrace
        , remoteStabilityTrace
        ]

    rulesForMaximumThroughput
        [ smallLocalMaximumThroughput
        , localMaximumThroughput
        , smallRemoteMaximumThroughput
        , remoteMaximumThroughput
        ]

    rulesForWriteEffect
        [ smallLocalWriteEffect
        , localWriteEffect
        , smallRemoteWriteEffect
        , remoteWriteEffect
        ]

    rulesForReplicationEffect
        [ smallLocalReplicationEffect
        , localReplicationEffect
        , smallRemoteReplicationEffect
        , remoteReplicationEffect
        ]


ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-trace-slice-analysis"

rulesForStabilityTraces :: [StabilityTraceCfg] -> Rules ()
rulesForStabilityTraces stcs = do
    rs <- catMaybes <$> mapM (rulesForTraceSliceAnalysis . experimentTraceSliceAnalysisConfig) stcs
    ruleForStabilityTraces ~> need rs

ruleForMaximumThroughputs :: String
ruleForMaximumThroughputs = "maximum-throughput-trace-slice-analysis"

rulesForMaximumThroughput :: [MaximumThroughputCfg] -> Rules ()
rulesForMaximumThroughput stcs = do
    rs <- catMaybes <$> mapM (rulesForTraceSliceAnalysis . experimentTraceSliceAnalysisConfig) stcs
    ruleForMaximumThroughputs ~> need rs

ruleForWriteEffects :: String
ruleForWriteEffects = "write-effect-trace-slice-analysis"

rulesForWriteEffect :: [WriteEffectCfg] -> Rules ()
rulesForWriteEffect stcs = do
    rs <- catMaybes <$> mapM (rulesForTraceSliceAnalysis . experimentTraceSliceAnalysisConfig) stcs
    ruleForWriteEffects ~> need rs

ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-trace-slice-analysis"

rulesForReplicationEffect :: [ReplicationEffectCfg] -> Rules ()
rulesForReplicationEffect stcs = do
    rs <- catMaybes <$> mapM (rulesForTraceSliceAnalysis . experimentTraceSliceAnalysisConfig) stcs
    ruleForReplicationEffects ~> need rs

experimentAnalysisCfgRule :: ExperimentConfig a => a -> String
experimentAnalysisCfgRule cfg = experimentTarget cfg ++ "-trace-slice-analysis"

experimentTraceSliceAnalysisConfig :: ExperimentConfig a => a -> TraceSliceAnalysisCfg
experimentTraceSliceAnalysisConfig ecf = TraceSliceAnalysisCfg
    { analysisTarget = experimentAnalysisCfgRule ecf
    , summaryLocationsPath = resultSummariesLocationFile ecf
    , analysisOutDir = analysisPlotsDir
    }

useTraceSlicePlotsInReport :: ExperimentConfig a => a -> Int -> Rules ()
useTraceSlicePlotsInReport ecf i
    = traceSlicePlotsFor ecf >>= (`usePlotsInReport` i)

dependOnTraceSlicePlotsForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnTraceSlicePlotsForReport ecf i
    = traceSlicePlotsFor ecf >>= (`dependOnPlotsForReport` i)

traceSlicePlotsFor :: (MonadIO m, ExperimentConfig a) => a -> m [FilePath]
traceSlicePlotsFor ecf = do
    let anCfg = experimentTraceSliceAnalysisConfig ecf
    slocs <- readResultsSummaryLocationsForCfg ecf
    summaries <- mapM readResultsSummary slocs
    return $ concatMap (traceSlicePlotsForSingleExperiment anCfg) summaries

rulesForTraceSliceAnalysis :: TraceSliceAnalysisCfg -> Rules (Maybe String)
rulesForTraceSliceAnalysis tsa@TraceSliceAnalysisCfg{..} = do
    let rslf = summaryLocationsPath
    onlyIfFileExists rslf $ do
        summaryPaths <- readResultsSummaryLocations rslf
        pss <- forM summaryPaths $ \summaryPath -> do
            ers@ExperimentResultSummary{..} <- readResultsSummary summaryPath
            let dFile = durationsFile erMiddleResultsFile
                adFile = absDurationsFile erMiddleResultsFile
                rdFile = relDurationsFile erMiddleResultsFile

            dFile %> \outFile -> do
                need [erMiddleResultsFile]
                size <- liftIO $ withFile erMiddleResultsFile ReadMode hFileSize
                -- 90 characters in the header line, then 95 characters per line
                let totalLines = (size - 90) `div` 95
                let window = totalLines `div` 20

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

            aplots <- traceSliceAnalysisOf tsa ers adFile "absolute"
            rplots <- traceSliceAnalysisOf tsa ers rdFile "relative"
            return $ aplots ++ rplots

        analysisTarget ~> need (concat pss)
        return analysisTarget

durationsFile :: FilePath -> FilePath
durationsFile f = analysisTmpDir </> dropExtensions (takeFileName f) ++ "-durations" <.> csvExt

absDurationsFile :: FilePath -> FilePath
absDurationsFile f = analysisTmpDir </> dropExtensions (takeFileName f) ++ "-absolute-durations" <.> csvExt

relDurationsFile :: FilePath -> FilePath
relDurationsFile f = analysisTmpDir </> dropExtensions (takeFileName f) ++ "-relative-durations" <.> csvExt
