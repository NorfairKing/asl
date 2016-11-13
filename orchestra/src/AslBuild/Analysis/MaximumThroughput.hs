{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.MaximumThroughput where

import           Control.Monad
import           Data.List
import           Data.Monoid
import           GHC.Generics

import           Development.Shake
import           Development.Shake.FilePath

import           Data.Csv
import qualified Data.Vector                            as V
import           Statistics.Sample

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Utils
import           AslBuild.Client
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.Baseline
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Reports.Common
import           AslBuild.Utils

throughputAnalysisRule :: String
throughputAnalysisRule = "throughput-analysis"

throughputAnalysisRules :: Rules ()
throughputAnalysisRules = do
    throughputAnalysisRule ~> need
        [ ruleForBaselines
        , ruleForMaximumThroughputs
        , ruleForWriteEffects
        , ruleForReplicationEffects
        , ruleForStabilityTraces
        ]

    let throughputAnalysisSubrules :: ExperimentConfig a => String -> [a] -> Rules ()
        throughputAnalysisSubrules = subRules rulesForThroughputAnalysis

    throughputAnalysisSubrules ruleForBaselines allBaselineExperiments
    throughputAnalysisSubrules ruleForStabilityTraces allStabilityTraceExperiments
    throughputAnalysisSubrules ruleForMaximumThroughputs allMaximumThroughputExperiments
    throughputAnalysisSubrules ruleForWriteEffects allWriteEffectExperiments
    throughputAnalysisSubrules ruleForReplicationEffects allReplicationEffectExperiments

ruleForBaselines :: String
ruleForBaselines = "baseline-throughput-analysis"

ruleForMaximumThroughputs :: String
ruleForMaximumThroughputs = "maximum-throughput-throughput-analysis"

ruleForWriteEffects :: String
ruleForWriteEffects = "write-effect-throughput-analysis"

ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-throughput-analysis"

ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-throughput-analysis"

ruleForThroughputAnalysis :: ExperimentConfig a => a -> String
ruleForThroughputAnalysis mtc
    = experimentTarget mtc ++ "-throughput-analysis"

throughputAnalysisPlotsFor :: ExperimentConfig a => a -> [FilePath]
throughputAnalysisPlotsFor mtc = [maximumThroughputPrefixFor mtc <.> pngExt]

simplifiedCsvFor :: ExperimentConfig a => a -> FilePath
simplifiedCsvFor mtc = experimentAnalysisTmpDir mtc </> "simplified.csv"

maximumThroughputPrefixFor :: ExperimentConfig a => a -> String
maximumThroughputPrefixFor mtc
    = experimentPlotsDir mtc </> experimentTarget mtc ++ "-maximum-throughput"

useThroughputAnalysisPlotsInReport :: ExperimentConfig a => a -> Int -> Rules ()
useThroughputAnalysisPlotsInReport stc
    = usePlotsInReport $ throughputAnalysisPlotsFor stc

dependOnThroughputAnalysisPlotsForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnThroughputAnalysisPlotsForReport stc
    = dependOnPlotsForReport $ throughputAnalysisPlotsFor stc

rulesForThroughputAnalysis :: ExperimentConfig a => a -> Rules (Maybe String)
rulesForThroughputAnalysis mtc = onlyIfResultsExist mtc $ do
    let plots = throughputAnalysisPlotsFor mtc

    let simpleCsv = simplifiedCsvFor mtc
    simpleCsv %> \_ -> do
        summaryPaths <- readResultsSummaryLocations $ resultSummariesLocationFile mtc
        eels <- forP summaryPaths $ \summaryPath -> do
            ExperimentResultSummary{..} <- readResultsSummary summaryPath
            es <- readExperimentSetup erSetupFile
            crs <- forP erClientResultsFiles readClientResults
            return $ simplifiedCsvLine es crs

        case sequence eels of
            Left err -> fail $ "Failed to make simplified csv file: " ++ err
            Right ls -> writeCSV simpleCsv ls

    plots &%> \_ -> do
        need [maximumThroughputAnalysisScript, simpleCsv, commonRLib]
        need [rBin]
        rScript maximumThroughputAnalysisScript commonRLib simpleCsv $
            maximumThroughputPrefixFor mtc

    let analysisTarget = ruleForThroughputAnalysis mtc
    analysisTarget ~> need plots
    return analysisTarget

simplifiedCsvLine
    :: ExperimentSetup
    -> [ClientResults]
    -> Either String MTSimpleLine
simplifiedCsvLine ExperimentSetup{..} crs = do
    -- Each client should have a TPS value per second of runtime.
    -- First we add those up per client, as if there was just one big client.
    tss <- forM logs $ \MemaslapLog{..} ->
        forM triples $ \triple -> do
            gStats <- maybeToEither "Get stats not found" $ getStats triple
            let stats = periodStats gStats
            return $ Sum $ tps stats

    when (length (nub tss) /= length tss) $
        Left $ "Triples did not match up: " ++ show (map length tss)

    let tpss = map getSum $ zipCombineLists tss
    let tpsDoubleVector = V.fromList $ map fromIntegral tpss
    let avgTps = floor $ mean tpsDoubleVector
    let stdDevTps = floor $ stdDev tpsDoubleVector

    pure MTSimpleLine
        { middleThreads = mts
        , clientConcurrencies = ccs
        , throughput = avgTps
        , standardDeviation = stdDevTps
        }
  where
    mts = case backendSetup of
        Left _ -> 0
        Right (middleSetup, _) -> mwNrThreads $ mMiddlewareFlags middleSetup
    ccs = sum $ map (msConcurrency . msFlags . cMemaslapSettings) clientSetups
    logs = map crLog crs

maximumThroughputAnalysisScript :: FilePath
maximumThroughputAnalysisScript = analysisDir </> "analyze_maximum_throughput.r"

data MTSimpleLine
    = MTSimpleLine
    { middleThreads       :: Int
    , clientConcurrencies :: Int
    , throughput          :: Int
    , standardDeviation   :: Int
    } deriving (Show, Eq, Generic)

instance ToNamedRecord MTSimpleLine where
    toNamedRecord MTSimpleLine{..} = namedRecord
        [ "threads" .= middleThreads
        , "conc" .= clientConcurrencies
        , "avgTps" .= throughput
        , "stdTps" .= standardDeviation
        ]

instance DefaultOrdered MTSimpleLine where
    headerOrder _ = header
        [ "threads"
        , "conc"
        , "avgTps"
        , "stdTps"
        ]

