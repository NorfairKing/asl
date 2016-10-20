{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis.StabilityTrace where

import           Data.List

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Constants
import           AslBuild.StabilityTrace

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
    kind <- ["read", "write"]
    return $ analysisOutDir </> intercalate "-" [filePrefix, kind, "resp"] <.> pngExt

smallLocalStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
smallLocalStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = smallLocalStabilityTrace
    , filePrefix = "small-local-stability-trace"
    , analysisOutDir = analysisDir
    }

localStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
localStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = localStabilityTrace
    , filePrefix = "local-stability-trace"
    , analysisOutDir = report1PlotsDir
    }

bigLocalStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
bigLocalStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = bigLocalStabilityTrace
    , filePrefix = "big-local-stability-trace"
    , analysisOutDir = analysisDir
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
    -- , remoteStabilityTraceAnalysis
    ]

allStabilityTracePlots :: [FilePath]
allStabilityTracePlots = concatMap plotsForStabilityTrace allStabilityTraceAnalyses

stabilityTraceAnalysisRules :: Rules ()
stabilityTraceAnalysisRules = do
    stabilityTraceAnalysisRule ~> need allStabilityTracePlots
    mapM_ stabilityTraceAnalysisRuleFor allStabilityTraceAnalyses

stabilityTraceAnalysisRuleFor :: StabilityTraceAnalysisCfg -> Rules ()
stabilityTraceAnalysisRuleFor bac@StabilityTraceAnalysisCfg{..} = plotsForStabilityTrace bac &%> \_ -> do
    let results = csvOutFile experiment
    resultsExist <- doesFileExist results

    need $ stabilityTraceAnalysisScript : [results | not resultsExist] -- Do not depend on results if they exist already.

    need [rBin]
    needRLibs ["pkgmaker"]
    needRLibs ["caTools"]
    unit $ rScript stabilityTraceAnalysisScript results filePrefix analysisOutDir
