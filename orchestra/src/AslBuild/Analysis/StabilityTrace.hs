{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis.StabilityTrace where

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

plotsFor :: StabilityTraceAnalysisCfg -> [FilePath]
plotsFor StabilityTraceAnalysisCfg{..} = []
    -- return $ analysisOutDir </> intercalate "-" [filePrefix, prefix, show nrclients] <.> pngExt

smallLocalStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
smallLocalStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = smallLocalStabilityTrace
    , filePrefix = "small-local-stabilityTrace-experiment"
    , analysisOutDir = analysisDir
    }

localStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
localStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = localStabilityTrace
    , filePrefix = "local-stabilityTrace-experiment"
    , analysisOutDir = analysisDir
    }

bigLocalStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
bigLocalStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = bigLocalStabilityTrace
    , filePrefix = "big-local-stabilityTrace-experiment"
    , analysisOutDir = analysisDir
    }

remoteStabilityTraceAnalysis :: StabilityTraceAnalysisCfg
remoteStabilityTraceAnalysis = StabilityTraceAnalysisCfg
    { experiment = remoteStabilityTrace
    , filePrefix = "remote-stabilityTrace-experiment"
    , analysisOutDir = report1PlotsDir
    }

allStabilityTraceAnalyses :: [StabilityTraceAnalysisCfg]
allStabilityTraceAnalyses =
    [ smallLocalStabilityTraceAnalysis
    , localStabilityTraceAnalysis
    , bigLocalStabilityTraceAnalysis
    , remoteStabilityTraceAnalysis
    ]

allStabilityTracePlots :: [FilePath]
allStabilityTracePlots = concatMap plotsFor allStabilityTraceAnalyses

stabilityTraceAnalysisRules :: Rules ()
stabilityTraceAnalysisRules = do
    stabilityTraceAnalysisRule ~> need allStabilityTracePlots
    mapM_ stabilityTraceAnalysisRuleFor allStabilityTraceAnalyses

stabilityTraceAnalysisRuleFor :: StabilityTraceAnalysisCfg -> Rules ()
stabilityTraceAnalysisRuleFor bac@StabilityTraceAnalysisCfg{..} = plotsFor bac &%> \_ -> do
    let results = csvOutFile experiment
    resultsExist <- doesFileExist results

    need $ stabilityTraceAnalysisScript : [results | not resultsExist] -- Do not depend on results if they exist already.

    need [rBin]
    needRLibs ["pkgmaker"]
    unit $ rScript stabilityTraceAnalysisScript results filePrefix analysisOutDir
