{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.MaximumThroughput where

import           Data.Maybe
import           GHC.Generics

import           Development.Shake
import           Development.Shake.FilePath

import           Data.Csv

import           AslBuild.Analysis.BuildR
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

maximumThroughputAnalysisRule :: String
maximumThroughputAnalysisRule = "maximum-throughput-analysis"

maximumThroughputAnalysisRules :: Rules ()
maximumThroughputAnalysisRules = do
    rs <- catMaybes <$> mapM rulesForMaximumThroughputExperiment allMaximumThroughputAnalyses

    maximumThroughputAnalysisRule ~> need rs

maximumThroughputRuleFor :: MaximumThroughputCfg -> String
maximumThroughputRuleFor mtc = experimentTarget mtc ++ "-maximum-throughput-analysis"

maximumThroughputPlotsFor :: MaximumThroughputCfg -> [FilePath]
maximumThroughputPlotsFor mtc = [maximumThroughputPrefixFor mtc <.> pngExt]

simplifiedCsvFor :: MaximumThroughputCfg -> FilePath
simplifiedCsvFor mtc = experimentAnalysisDir mtc </> "simplified.csv"

maximumThroughputPrefixFor :: MaximumThroughputCfg -> String
maximumThroughputPrefixFor mtc = experimentAnalysisDir mtc </> experimentTarget mtc ++ "-maximum-throughput"

useMaximumThroughputPlotsInReport :: MaximumThroughputCfg -> Int -> Rules ()
useMaximumThroughputPlotsInReport stc = usePlotsInReport $ maximumThroughputPlotsFor stc

dependOnMaximumThroughputPlotsForReport :: MaximumThroughputCfg -> Int -> Action ()
dependOnMaximumThroughputPlotsForReport stc = dependOnPlotsForReport $ maximumThroughputPlotsFor stc

rulesForMaximumThroughputExperiment :: MaximumThroughputCfg -> Rules (Maybe String)
rulesForMaximumThroughputExperiment mtc = onlyIfResultsExist mtc $ do
    let plots = maximumThroughputPlotsFor mtc

    let simpleCsv = simplifiedCsvFor mtc
    simpleCsv %> \_ -> do
        summaryPaths <- readResultsSummaryLocations $ resultSummariesLocationFile mtc
        ls <- forP summaryPaths $ \summaryPath -> do
            ExperimentResultSummary{..} <- readResultsSummary summaryPath
            ExperimentSetup{..} <- readExperimentSetup erSetupFile
            let mts = mwNrThreads $ mMiddlewareFlags middleSetup
            let ccs = maximum $ map (msConcurrency . msFlags . cMemaslapSettings) clientSetups
            crs <- forP erClientResultsFiles readClientResults
            let totalThroughput = sum $ map (finalTps . finalStats . crLog) crs
            return MTSimpleLine
                { middleThreads = mts
                , clientConcurrencies = ccs
                , throughput = totalThroughput
                }

        writeCSV simpleCsv ls

    plots &%> \_ -> do
        need [maximumThroughputAnalysisScript, simpleCsv]
        need [rBin]
        rScript maximumThroughputAnalysisScript simpleCsv $ maximumThroughputPrefixFor mtc

    let analysisTarget = maximumThroughputRuleFor mtc
    analysisTarget ~> need plots
    return analysisTarget

maximumThroughputAnalysisScript :: FilePath
maximumThroughputAnalysisScript = analysisDir </> "analyze_maximum_throughput.r"

data MTSimpleLine
    = MTSimpleLine
    { middleThreads       :: Int
    , clientConcurrencies :: Int
    , throughput          :: Int -- TODO show standard deviation as well.
    } deriving (Show, Eq, Generic)

instance ToNamedRecord MTSimpleLine where
instance DefaultOrdered MTSimpleLine
