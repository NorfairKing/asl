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

import           Pipes                               ((>->))
import qualified Pipes                               as P
import qualified Pipes.ByteString                    as PB
import qualified Pipes.Csv                           as P
import qualified Pipes.Prelude                       as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Script
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.StabilityTrace

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

ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-trace-slice-analysis"

rulesForStabilityTraces :: [StabilityTraceCfg] -> Rules ()
rulesForStabilityTraces stcs = do
    rs <- catMaybes <$> mapM (rulesForTraceSliceAnalysis . stabilityTraceTraceSliceAnalysisConfig) stcs

    ruleForStabilityTraces ~> need rs

traceSliceAnalysisCfgRule :: ExperimentConfig a => a -> String
traceSliceAnalysisCfgRule cfg = experimentTarget cfg ++ "-trace-slice-analysis"

stabilityTraceTraceSliceAnalysisConfig :: ExperimentConfig a => a -> TraceSliceAnalysisCfg
stabilityTraceTraceSliceAnalysisConfig ecf = TraceSliceAnalysisCfg
    { analysisTarget = traceSliceAnalysisCfgRule ecf
    , summaryLocationsPath = resultSummariesLocationFile ecf
    , analysisOutDir = analysisPlotsDir
    }

rulesForTraceSliceAnalysis :: TraceSliceAnalysisCfg -> Rules (Maybe String)
rulesForTraceSliceAnalysis tsa@TraceSliceAnalysisCfg{..} = do
    let rslf = summaryLocationsPath
    onlyIfFileExists rslf $ do
        summaryPaths <- readResultsSummaryLocations rslf
        pss <- forM summaryPaths $ \summaryPath -> do
            ers@ExperimentResultSummary{..} <- readResultsSummary summaryPath
            let dFile = durationsFile erMiddleResultsFile

            dFile %> \outFile ->
                liftIO $
                    withFile erMiddleResultsFile ReadMode $ \inHandle ->
                        withFile outFile WriteMode $ \outHandle ->
                            P.runEffect $
                                    P.decodeByName (PB.fromHandle inHandle)
                                >-> errorLogger
                                >-> timeTransformer
                                >-> P.drop 3
                                >-> meanTransformer 250
                                >-> lineTransformer
                                >-> P.encodeByName durationsLineHeader
                                >-> PB.toHandle outHandle


            traceSliceAnalysisOf tsa ers dFile

        analysisTarget ~> need (concat pss)
        return analysisTarget

durationsFile :: FilePath -> FilePath
durationsFile f = analysisTmpDir </> dropExtensions (takeFileName f) ++ "-durations" <.> csvExt
