{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.MaximumThroughput where

import           Control.Monad
import           Data.List
import           Data.Maybe
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
    rs <- catMaybes <$> mapM rulesForMaximumThroughputExperiment allMaximumThroughputExperiments
    maximumThroughputAnalysisRule ~> need rs

maximumThroughputRuleFor :: MaximumThroughputCfg -> String
maximumThroughputRuleFor mtc
    = experimentTarget mtc ++ "-maximum-throughput-analysis"

maximumThroughputPlotsFor :: MaximumThroughputCfg -> [FilePath]
maximumThroughputPlotsFor mtc = [maximumThroughputPrefixFor mtc <.> pngExt]

simplifiedCsvFor :: MaximumThroughputCfg -> FilePath
simplifiedCsvFor mtc = experimentAnalysisDir mtc </> "simplified.csv"

maximumThroughputPrefixFor :: MaximumThroughputCfg -> String
maximumThroughputPrefixFor mtc
    = experimentAnalysisDir mtc </> experimentTarget mtc ++ "-maximum-throughput"

useMaximumThroughputPlotsInReport :: MaximumThroughputCfg -> Int -> Rules ()
useMaximumThroughputPlotsInReport stc
    = usePlotsInReport $ maximumThroughputPlotsFor stc

dependOnMaximumThroughputPlotsForReport :: MaximumThroughputCfg -> Int -> Action ()
dependOnMaximumThroughputPlotsForReport stc
    = dependOnPlotsForReport $ maximumThroughputPlotsFor stc

rulesForMaximumThroughputExperiment :: MaximumThroughputCfg -> Rules (Maybe String)
rulesForMaximumThroughputExperiment mtc = onlyIfResultsExist mtc $ do
    let plots = maximumThroughputPlotsFor mtc

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
        need [maximumThroughputAnalysisScript, simpleCsv]
        need [rBin]
        rScript maximumThroughputAnalysisScript simpleCsv $
            maximumThroughputPrefixFor mtc

    let analysisTarget = maximumThroughputRuleFor mtc
    analysisTarget ~> need plots
    return analysisTarget

zipCombineLists :: Monoid a => [[a]] -> [a]
zipCombineLists = map mconcat . transpose

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither dv Nothing = Left dv
maybeToEither _ (Just v) = Right v

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
    mts = mwNrThreads $ mMiddlewareFlags middleSetup
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

