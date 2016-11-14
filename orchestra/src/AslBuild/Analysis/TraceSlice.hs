{-# LANGUAGE OverloadedStrings #-}
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

import qualified Data.Csv                               as CSV

import           Pipes                                  ((>->))
import qualified Pipes                                  as P
import qualified Pipes.ByteString                       as PB
import qualified Pipes.Csv                              as P
import qualified Pipes.Prelude                          as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Script
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Analysis.TraceSlice.Utils
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Reports.Common
import           AslBuild.Utils

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

experimentAnalysisCfgRule :: ExperimentConfig a => a -> String
experimentAnalysisCfgRule cfg = experimentTarget cfg ++ "-trace-slice-analysis"

useTraceSlicePlotsInReport :: ExperimentConfig a => a -> Int -> Rules ()
useTraceSlicePlotsInReport ecf i
    = traceSlicePlotsFor ecf >>= (`usePlotsInReport` i)

dependOnTraceSlicePlotsForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnTraceSlicePlotsForReport ecf i
    = traceSlicePlotsFor ecf >>= (`dependOnPlotsForReport` i)

traceSlicePlotsFor :: (MonadIO m, ExperimentConfig a) => a -> m [FilePath]
traceSlicePlotsFor = return . traceSlicePlotsForSingleExperiment

rulesForTraceSliceAnalysis :: ExperimentConfig a => a -> Rules (Maybe String)
rulesForTraceSliceAnalysis ecf = onlyIfResultsExist ecf $ do
    summaryPaths <- readResultsSummaryLocationsForCfg ecf
    let dFile = durationsFile ecf
        adFile = absDurationsFile ecf
        rdFile = relDurationsFile ecf
    dFile %> \outFile -> do
        durTups <- (catMaybes <$>) $ forM summaryPaths $ \summaryPath -> do
            ers <- readResultsSummary summaryPath
            case merMiddleResultsFile ers of
                Nothing -> return Nothing
                Just erMiddleResultsFile -> do
                    setup <- readExperimentSetupForSummary ers
                    avgDur <- avgDurations erMiddleResultsFile
                    return $ Just (setup, avgDur)

        liftIO $ withFile outFile WriteMode $ \outHandle ->
            P.runEffect $
                    P.each durTups
                >-> P.filter ((==1) . mwNrThreads . mMiddlewareFlags . fst . fromRight . backendSetup . fst)
                >-> durtupTransformer
                >-> P.encodeByName (CSV.headerOrder (undefined :: DurTup))
                >-> PB.toHandle outHandle

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

durationsFile :: ExperimentConfig a => a -> FilePath
durationsFile ecf = experimentAnalysisTmpDir ecf </> dropExtensions (takeFileName (resultSummariesLocationFile ecf)) ++ "-durations" <.> csvExt

absDurationsFile :: ExperimentConfig a => a -> FilePath
absDurationsFile ecf = experimentAnalysisTmpDir ecf </> dropExtensions (takeFileName (resultSummariesLocationFile ecf)) ++ "-absolute-durations" <.> csvExt

relDurationsFile :: ExperimentConfig a => a -> FilePath
relDurationsFile ecf = experimentAnalysisTmpDir ecf </> dropExtensions (takeFileName (resultSummariesLocationFile ecf)) ++ "-relative-durations" <.> csvExt
