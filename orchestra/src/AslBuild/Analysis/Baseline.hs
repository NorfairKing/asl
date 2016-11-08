{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis.Baseline where

import           Data.List
import           Data.Maybe

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiments.Baseline
import           AslBuild.Reports.Common

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
    , analysisOutDir = analysisPlotsDir
    }

localBaselineAnalysis :: BaselineAnalysisCfg
localBaselineAnalysis = BaselineAnalysisCfg
    { experiment = localBaselineExperiment
    , filePrefix = "local-baseline-experiment"
    , analysisOutDir = analysisPlotsDir
    }

remoteBaselineAnalysis :: BaselineAnalysisCfg
remoteBaselineAnalysis = BaselineAnalysisCfg
    { experiment = remoteBaselineExperiment
    , filePrefix = "remote-baseline-experiment"
    , analysisOutDir = reportPlotsDir 1
    }

smallRemoteBaselineAnalysis :: BaselineAnalysisCfg
smallRemoteBaselineAnalysis = BaselineAnalysisCfg
    { experiment = smallRemoteBaselineExperiment
    , filePrefix = "small-remote-baseline-experiment"
    , analysisOutDir = analysisPlotsDir
    }

allBaselineAnalyses :: [BaselineAnalysisCfg]
allBaselineAnalyses =
    [ smallLocalBaselineAnalysis
    , localBaselineAnalysis
    , smallRemoteBaselineAnalysis
    , remoteBaselineAnalysis
    ]

allBaselinePlots :: [FilePath]
allBaselinePlots = concatMap plotsForBaseline allBaselineAnalyses

baselineAnalysisRules :: Rules ()
baselineAnalysisRules = do
    ts <- catMaybes <$> mapM baselineAnalysisRulesFor allBaselineAnalyses
    baselineAnalysisRule ~> need ts

baselineAnalysisRuleFor :: BaselineAnalysisCfg -> String
baselineAnalysisRuleFor BaselineAnalysisCfg{..}
    = target experiment ++ "-analysis"

baselineAnalysisRulesFor :: BaselineAnalysisCfg -> Rules (Maybe String)
baselineAnalysisRulesFor bac@BaselineAnalysisCfg{..} = do
    let results = csvOutFile experiment
    onlyIfFileExists results $ do
        let plotsForThisBaseline = plotsForBaseline bac
        plotsForThisBaseline &%> \_ -> do
            needsToExist results
            need [baselineAnalysisScript]

            need [rBin]
            needRLibs ["pkgmaker"]
            needRLibs ["igraph"]
            unit $ rScript baselineAnalysisScript results filePrefix analysisOutDir

        let target = baselineAnalysisRuleFor bac
        target ~> need plotsForThisBaseline
        return target
