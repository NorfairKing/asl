{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AslBuild.Analysis.StabilityTrace where

import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.Csv
import Data.List
import Data.Maybe

import Development.Shake
import Development.Shake.FilePath

import AslBuild.Analysis.BuildR
import AslBuild.Analysis.Utils
import AslBuild.CommonActions
import AslBuild.Constants
import AslBuild.Experiment
import AslBuild.Experiments.StabilityTrace
import AslBuild.Memaslap
import AslBuild.Reports.Common
import AslBuild.Utils

stabilityTraceAnalysisScript :: FilePath
stabilityTraceAnalysisScript = analysisDir </> "analyze_stability_trace.r"

stabilityTraceAnalysisRule :: String
stabilityTraceAnalysisRule = "stability-trace-analysis"

plotsForStabilityTrace :: StabilityTraceCfg -> [FilePath]
plotsForStabilityTrace stc = do
    kind <- ["resp", "tps"]
    return $
        experimentPlotsDir stc </> intercalate "-" [stabilityTracePrefixFor stc, kind] <.> pngExt

stabilityTracePrefixFor :: StabilityTraceCfg -> FilePath
stabilityTracePrefixFor stc = experimentTarget stc ++ "-stability-trace-analysis"

stabilityTraceAnalysisRules :: Rules ()
stabilityTraceAnalysisRules = do
    ts <- catMaybes <$> mapM stabilityTraceAnalysisRulesFor allStabilityTraceExperiments
    stabilityTraceAnalysisRule ~> need ts

stabilityTraceAnalysisRuleFor :: StabilityTraceCfg -> String
stabilityTraceAnalysisRuleFor stc = experimentTarget stc ++ "-analysis"

useStabilityTracePlotsInReport :: StabilityTraceCfg -> Int -> Rules ()
useStabilityTracePlotsInReport stc i = forM_ (plotsForStabilityTrace stc) (`usePlotInReport` i)

dependOnStabilityTracePlotsForReport :: StabilityTraceCfg -> Int -> Action ()
dependOnStabilityTracePlotsForReport stc = dependOnPlotsForReport $ plotsForStabilityTrace stc

stabilityTraceAnalysisRulesFor :: StabilityTraceCfg -> Rules (Maybe String)
stabilityTraceAnalysisRulesFor stc =
    onlyIfResultsExist stc $ do
        let summaryLocationsFile = resultSummariesLocationFile stc
        let simpleCsvFile = experimentAnalysisDir stc </> "simple.csv"
        simpleCsvFile %> \_
        -- Don't depend on the summary locations file if it exists
         -> do
            needsToExist summaryLocationsFile
        -- TODO fix this to combine the files if that's necessary.
            [summaryFile:_] <- readResultsSummaryLocations summaryLocationsFile
            ExperimentResultSummary {..} <- readResultsSummary summaryFile
            let resfiles = map (localClientResultsFile stc) erClientLogFiles
            need resfiles
            logs <- forP resfiles readJSON
            putLoud "Converting logfiles to a simple CSV file."
            let statistics :: MemaslapLog -> [Statistics]
                statistics = map (periodStats . bothStats) . triples
            let tpsTuples :: MemaslapLog -> [(Int, Statistics)]
                tpsTuples = zip [1 ..] . statistics
            let tupsList :: [(Int, [(Int, Statistics)])]
                tupsList = zip [1 ..] $ map tpsTuples logs
            let withClientSimplePoints :: [SimplifiedPoint]
                withClientSimplePoints =
                    concatMap
                        (\(client, simplePoints) ->
                             map (uncurry (toSimplePoint client)) simplePoints)
                        tupsList
            let enc = simpleCsv withClientSimplePoints
            liftIO $ LB.writeFile simpleCsvFile enc
        let plotsForThisTrace = plotsForStabilityTrace stc
        plotsForThisTrace &%> \_ -> do
            need [simpleCsvFile, stabilityTraceAnalysisScript]
            need [rBin]
            needRLibs ["pkgmaker"]
            needRLibs ["caTools"]
            rScript
                stabilityTraceAnalysisScript
                simpleCsvFile
                (stabilityTracePrefixFor stc)
                (experimentPlotsDir stc)
        let rule = stabilityTraceAnalysisRuleFor stc
        rule ~> need plotsForThisTrace
        return rule

simpleCsv :: [SimplifiedPoint] -> LB.ByteString
simpleCsv sps = encodeByName (header ["client", "second", "tps", "avg", "std"]) sps

toSimplePoint :: Int -> Int -> Statistics -> SimplifiedPoint
toSimplePoint client ix Statistics {..} =
    SimplifiedPoint {second = ix, sClient = client, sTps = tps, sAvg = avgUs, sStd = std}

data SimplifiedPoint = SimplifiedPoint
    { second :: Int
    , sClient :: Int
    , sTps :: Int
    , sAvg :: Int
    , sStd :: Double
    } deriving (Show, Eq)

instance ToNamedRecord SimplifiedPoint where
    toNamedRecord SimplifiedPoint {..} =
        namedRecord
            ["second" .= second, "client" .= sClient, "tps" .= sTps, "avg" .= sAvg, "std" .= sStd]
-- clientResultsDir :: StabilityTraceCfg -> FilePath
-- clientResultsDir StabilityTraceCfg{..} = experimentTarget experiment
