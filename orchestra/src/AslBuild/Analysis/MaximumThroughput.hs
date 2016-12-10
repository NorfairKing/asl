{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.MaximumThroughput where

import           Data.List
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics

import           Development.Shake
import           Development.Shake.FilePath

import           Data.Csv
import qualified Data.Vector                            as V

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Client
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
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
        [ ruleForMaximumThroughputs
        ]

    subRules rulesForThroughputAnalysis ruleForMaximumThroughputs allMaximumThroughputExperiments

ruleForMaximumThroughputs :: String
ruleForMaximumThroughputs = "maximum-throughput-throughput-analysis"

ruleForThroughputAnalysis :: ExperimentConfig a => a -> String
ruleForThroughputAnalysis mtc
    = experimentTarget mtc ++ "-throughput-analysis"

throughputAnalysisPlotsFor :: MaximumThroughputCfg -> [FilePath]
throughputAnalysisPlotsFor mtc = do
    thds <- nub (map (show . fst) $ threadConcTups mtc)
    metric <- ["tps", "resp"]
    return $ intercalate "-" [maximumThroughputPrefixFor mtc, thds, metric] <.> pngExt

simplifiedCsvFor :: ExperimentConfig a => a -> FilePath
simplifiedCsvFor mtc = experimentAnalysisTmpDir mtc </> "simplified.csv"

maximumThroughputPrefixFor :: MaximumThroughputCfg -> String
maximumThroughputPrefixFor mtc
    = experimentPlotsDir mtc </> experimentTarget mtc ++ "-maximum-throughput"

useThroughputAnalysisPlotsInReport :: MaximumThroughputCfg -> Int -> Rules ()
useThroughputAnalysisPlotsInReport stc
    = usePlotsInReport $ throughputAnalysisPlotsFor stc

dependOnThroughputAnalysisPlotsForReport :: MaximumThroughputCfg -> Int -> Action ()
dependOnThroughputAnalysisPlotsForReport stc
    = dependOnPlotsForReport $ throughputAnalysisPlotsFor stc

rulesForThroughputAnalysis :: MaximumThroughputCfg -> Rules (Maybe String)
rulesForThroughputAnalysis mtc = onlyIfResultsExist mtc $ do
    let plots = throughputAnalysisPlotsFor mtc

    let simpleCsv = simplifiedCsvFor mtc
    simpleCsv %> \_ -> do
        summaryPaths <- readResultsSummaryLocations $ resultSummariesLocationFile mtc
        let combinedResultsFiles = map (combineClientResultsFile mtc) summaryPaths
        need combinedResultsFiles
        ls <- forP summaryPaths $ \summaryPath -> do
            ers <- readResultsSummary summaryPath
            es <- readExperimentSetupForSummary ers
            res <- readCombinedClientResults $ combineClientResultsFile mtc summaryPath
            return $ simplifiedCsvLine es res

        writeCSV simpleCsv ls

    plots &%> \_ -> do
        need [maximumThroughputAnalysisScript, simpleCsv, commonRLib]
        need [rBin]
        needRLibs ["ggplot2"]
        rScript maximumThroughputAnalysisScript commonRLib simpleCsv $
            maximumThroughputPrefixFor mtc

    let analysisTarget = ruleForThroughputAnalysis mtc
    analysisTarget ~> need plots
    return analysisTarget

simplifiedCsvLine
    :: ExperimentSetup
    -> MemaslapClientResults
    -> MTSimpleLine
simplifiedCsvLine ExperimentSetup{..} MemaslapClientResults{..} =
    let (ms, _) = fromRight backendSetup
    in MTSimpleLine
        { middleThreads = mwNrThreads $ mMiddlewareFlags ms
        , clientConcurrencies = sum $ map (msConcurrency . msFlags . cMemaslapSettings) clientSetups
        , throughput = fromJust $ getResults tpsResults
        , responseTime = fromJust $ getResults respResults
        }

maximumThroughputAnalysisScript :: FilePath
maximumThroughputAnalysisScript = analysisDir </> "analyze_maximum_throughput.r"

data MTSimpleLine
    = MTSimpleLine
    { middleThreads       :: Int
    , clientConcurrencies :: Int
    , throughput          :: Avg
    , responseTime        :: Avg
    } deriving (Show, Eq, Generic)

instance ToNamedRecord MTSimpleLine where
    toNamedRecord MTSimpleLine{..} = namedRecord
        [ "threads" .= middleThreads
        , "conc" .= clientConcurrencies
        ] <> mapKeys (<> "Resp") (toNamedRecord responseTime)
          <> mapKeys (<> "Tps") (toNamedRecord throughput)

instance DefaultOrdered MTSimpleLine where
    headerOrder _ = header
        [ "threads"
        , "conc"
        ] <> V.map (<> "Resp") (headerOrder (undefined :: Avg))
          <> V.map (<> "Tps") (headerOrder (undefined :: Avg))

