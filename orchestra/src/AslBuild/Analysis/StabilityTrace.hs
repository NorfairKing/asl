{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.StabilityTrace where

import           Control.Monad
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as LB
import           Data.Csv
import           Data.List

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Constants
import           AslBuild.Memaslap
import           AslBuild.StabilityTrace
import           AslBuild.Utils

stabilityTraceAnalysisScript :: FilePath
stabilityTraceAnalysisScript = analysisDir </> "analyze_stability_trace.r"

stabilityTraceAnalysisRule :: String
stabilityTraceAnalysisRule = "stability-trace-analysis"

data StabilityTraceAnalysisCfg
    = StabilityTraceAnalysisCfg
    { experiment     :: StabilityTraceCfg
    , filePrefix     :: FilePath
    , analysisOutDir :: FilePath
    }

plotsForStabilityTrace :: StabilityTraceAnalysisCfg -> [FilePath]
plotsForStabilityTrace StabilityTraceAnalysisCfg{..} = do
    kind <- ["resp", "tps"]
    return $ analysisOutDir </> intercalate "-" [filePrefix, kind] <.> pngExt

smallLocalStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
smallLocalStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = smallLocalStabilityTrace
    , filePrefix = "small-local-stability-trace"
    , analysisOutDir = analysisPlotsDir
    }

localStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
localStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = localStabilityTrace
    , filePrefix = "local-stability-trace"
    , analysisOutDir = analysisPlotsDir
    }

bigLocalStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
bigLocalStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = bigLocalStabilityTrace
    , filePrefix = "big-local-stability-trace"
    , analysisOutDir = analysisPlotsDir
    }

smallRemoteStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
smallRemoteStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = smallRemoteStabilityTrace
    , filePrefix = "small-remote-stability-trace"
    , analysisOutDir = analysisPlotsDir
    }

remoteStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
remoteStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = remoteStabilityTrace
    , filePrefix = "remote-stability-trace"
    , analysisOutDir = report1PlotsDir
    }

allStabilityTraceAnalyses :: [StabilityTraceAnalysisCfg]
allStabilityTraceAnalyses =
    [ smallLocalStabilityTraceAnalysis
    , localStabilityTraceAnalysis
    -- , bigLocalStabilityTraceAnalysis
    , smallRemoteStabilityTraceAnalysis
    , remoteStabilityTraceAnalysis
    ]

allStabilityTracePlots :: [FilePath]
allStabilityTracePlots = concatMap plotsForStabilityTrace allStabilityTraceAnalyses

stabilityTraceAnalysisRules :: Rules ()
stabilityTraceAnalysisRules = do
    stabilityTraceAnalysisRule ~> need allStabilityTracePlots
    mapM_ stabilityTraceAnalysisRuleFor allStabilityTraceAnalyses

stabilityTraceAnalysisRuleFor :: StabilityTraceAnalysisCfg -> Rules ()
stabilityTraceAnalysisRuleFor bac@StabilityTraceAnalysisCfg{..} = do
    let t = target experiment
    let adir = tmpDir </> target experiment

    let readLogs :: Action [StabilityTraceExperimentResults]
        readLogs = do
            files <- absFilesInDir (resultsDir </> t) ["*"]
            need files
            forM files $ \file -> do
                contents <- liftIO $ LB.readFile file
                putLoud $ unlines $ "Reading logfiles:" : files
                case A.eitherDecode contents of
                    Left err -> fail $ unwords
                        [ "Failed to decode contents of"
                        , file
                        , ", error:"
                        , err
                        ]
                    Right res -> return res

    let simpleCsvFile = adir </> "simple.csv"
    simpleCsvFile %> \_ -> do
        logs <- readLogs
        putLoud "Converting logfiles to a simple CSV file."
        let statistics :: StabilityTraceExperimentResults -> [Statistics]
            statistics = map (periodStats . bothStats) . triples . sterMemaslapLog
        let tpsTuples :: StabilityTraceExperimentResults -> [(Int, Statistics)]
            tpsTuples = zip [1..] . statistics
        let tupsList :: [(Int, [(Int, Statistics)])]
            tupsList = zip [1..] $ map tpsTuples logs
        let withClientSimplePoints :: [SimplifiedPoint]
            withClientSimplePoints = concatMap (\(client, simplePoints) -> map (uncurry (toSimplePoint client)) simplePoints) tupsList
        let enc = simpleCsv withClientSimplePoints
        liftIO $ LB.writeFile simpleCsvFile enc

    plotsForStabilityTrace bac &%> \_ -> do
        let results = csvOutFile experiment
        resultsExist <- doesFileExist results

        need $ stabilityTraceAnalysisScript : [results | not resultsExist] -- Do not depend on results if they exist already.

        need [rBin]
        needRLibs ["pkgmaker"]
        needRLibs ["caTools"]

        need [simpleCsvFile]
        unit $ rScript stabilityTraceAnalysisScript simpleCsvFile filePrefix analysisOutDir

simpleCsv :: [SimplifiedPoint] -> LB.ByteString
simpleCsv sps = encodeByName (header ["client", "second", "tps", "avg", "std"]) sps

toSimplePoint :: Int -> Int -> Statistics -> SimplifiedPoint
toSimplePoint client ix Statistics{..} = SimplifiedPoint
    { second = ix
    , sClient = client
    , sTps = tps
    , sAvg = avgUs
    , sStd = stdDev
    }

data SimplifiedPoint
    = SimplifiedPoint
    { second  :: Int
    , sClient :: Int
    , sTps    :: Int
    , sAvg    :: Int
    , sStd    :: Double
    } deriving (Show, Eq)

instance ToNamedRecord SimplifiedPoint where
    toNamedRecord SimplifiedPoint{..} = namedRecord
            [ "second" .= second
            , "client" .= sClient
            , "tps" .= sTps
            , "avg" .= sAvg
            , "std" .= sStd
            ]

clientResultsDir :: StabilityTraceAnalysisCfg -> FilePath
clientResultsDir StabilityTraceAnalysisCfg{..} = target experiment
