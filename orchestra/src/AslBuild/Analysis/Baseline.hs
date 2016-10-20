{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis.Baseline where

import           Data.List

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Baseline
import           AslBuild.Baseline.Types
import           AslBuild.Constants

baselineAnalysisScript :: FilePath
baselineAnalysisScript = analysisDir </> "analyze_baseline.r"

baselineAnalysisRule :: String
baselineAnalysisRule = "baseline-analysis"

data BaselineAnalysisCfg
    = BaselineAnalysisCfg
    { experiment     :: BaselineExperimentRuleCfg
    , filePrefix     :: FilePath
    , analysisOutDir :: FilePath
    }

plotsForBaseline :: BaselineAnalysisCfg -> [FilePath]
plotsForBaseline BaselineAnalysisCfg{..} = map
    (\f -> analysisOutDir </> intercalate "-" [filePrefix, f] <.> pngExt)
    [ "avg"
    , "tps"
    ]

smallLocalBaselineAnalysis :: BaselineAnalysisCfg
smallLocalBaselineAnalysis = BaselineAnalysisCfg
    { experiment = smallLocalBaselineExperiment
    , filePrefix = "small-local-baseline-experiment"
    , analysisOutDir = analysisDir
    }

localBaselineAnalysis :: BaselineAnalysisCfg
localBaselineAnalysis = BaselineAnalysisCfg
    { experiment = localBaselineExperiment
    , filePrefix = "local-baseline-experiment"
    , analysisOutDir = analysisDir
    }

bigLocalBaselineAnalysis :: BaselineAnalysisCfg
bigLocalBaselineAnalysis = BaselineAnalysisCfg
    { experiment = bigLocalBaselineExperiment
    , filePrefix = "big-local-baseline-experiment"
    , analysisOutDir = analysisDir
    }

remoteBaselineAnalysis :: BaselineAnalysisCfg
remoteBaselineAnalysis = BaselineAnalysisCfg
    { experiment = remoteBaselineExperiment
    , filePrefix = "remote-baseline-experiment"
    , analysisOutDir = report1PlotsDir
    }

smallRemoteBaselineAnalysis :: BaselineAnalysisCfg
smallRemoteBaselineAnalysis = BaselineAnalysisCfg
    { experiment = smallRemoteBaselineExperiment
    , filePrefix = "small-remote-baseline-experiment"
    , analysisOutDir = analysisDir
    }

allBaselineAnalyses :: [BaselineAnalysisCfg]
allBaselineAnalyses =
    [ smallLocalBaselineAnalysis
    , localBaselineAnalysis
    , bigLocalBaselineAnalysis
    , remoteBaselineAnalysis
    , smallRemoteBaselineAnalysis
    ]

allBaselinePlots :: [FilePath]
allBaselinePlots = concatMap plotsForBaseline allBaselineAnalyses

baselineAnalysisRules :: Rules ()
baselineAnalysisRules = do
    baselineAnalysisRule ~> need allBaselinePlots
    mapM_ baselineAnalysisRuleFor allBaselineAnalyses

baselineAnalysisRuleFor :: BaselineAnalysisCfg -> Rules ()
baselineAnalysisRuleFor bac@BaselineAnalysisCfg{..} = plotsForBaseline bac &%> \_ -> do
    let results = csvOutFile experiment
    resultsExist <- doesFileExist results

    need $ baselineAnalysisScript : [results | not resultsExist] -- Do not depend on results if they exist already.

    need [rBin]
    needRLibs ["pkgmaker"]
    needRLibs ["igraph"]
    unit $ rScript baselineAnalysisScript results filePrefix analysisOutDir
