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
import qualified Pipes                                  as P
import qualified Pipes.ByteString                       as PB
import qualified Pipes.Csv                              as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Script
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Reports.Common

traceSliceAnalysisRule :: String
traceSliceAnalysisRule = "trace-slice-analysis"

traceSliceAnalysisRules :: Rules ()
traceSliceAnalysisRules = do
    traceSliceAnalysisRule ~> need
        [ ruleForStabilityTraces
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

            dFile %> \outFile -> do
                size <- liftIO $ withFile erMiddleResultsFile ReadMode hFileSize
                let totalLines = (size - 90) `div` 95 -- 90 characters in the header line, then 95 characters per line
                let window = min 250 (totalLines `div` 20) -- A maximum of 250 so that this isn't too slow.

                putLoud $ unwords
                    [ "Distilling trace slice data from"
                    , erMiddleResultsFile
                    , "into"
                    , outFile
                    , "with window size"
                    , show window
                    ]

                liftIO $
                    withFile erMiddleResultsFile ReadMode $ \inHandle ->
                        withFile outFile WriteMode $ \outHandle ->
                            P.runEffect $
                                    P.decodeByName (PB.fromHandle inHandle)
                                >-> errorLogger
                                >-> timeTransformer
                                >-> meanTransformer window
                                >-> lineTransformer
                                >-> P.encodeByName durationsLineHeader
                                >-> PB.toHandle outHandle


            traceSliceAnalysisOf tsa ers dFile

        analysisTarget ~> need (concat pss)
        return analysisTarget

durationsFile :: FilePath -> FilePath
durationsFile f = analysisTmpDir </> dropExtensions (takeFileName f) ++ "-durations" <.> csvExt
