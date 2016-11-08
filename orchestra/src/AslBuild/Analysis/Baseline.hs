module AslBuild.Analysis.Baseline where

import           Control.Monad
import           Data.List
import           Data.Maybe

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiments.Baseline as B
import           AslBuild.Reports.Common

baselineAnalysisScript :: FilePath
baselineAnalysisScript = analysisDir </> "analyze_baseline.r"

baselineAnalysisRule :: String
baselineAnalysisRule = "baseline-analysis"

baselinePrefixFor :: BaselineExperimentRuleCfg -> FilePath
baselinePrefixFor bac = B.target bac ++ "-baseline-analysis"

plotsForBaseline :: BaselineExperimentRuleCfg -> [FilePath]
plotsForBaseline bac = map
    (\f -> analysisPlotsDir </> intercalate "-" [baselinePrefixFor bac, f] <.> pngExt)
        [ "avg"
        , "tps"
        ]

baselineAnalysisRules :: Rules ()
baselineAnalysisRules = do
    ts <- catMaybes <$> mapM baselineAnalysisRulesFor allBaselineExperiments
    baselineAnalysisRule ~> need ts

baselineAnalysisRuleFor :: BaselineExperimentRuleCfg -> String
baselineAnalysisRuleFor bac
    = B.target bac ++ "-baseline-analysis"

useBaselinePlotsInReport :: BaselineExperimentRuleCfg -> Int -> Rules ()
useBaselinePlotsInReport bac i = forM_ (plotsForBaseline bac) (`usePlotInReport` i)

dependOnBaselinePlotsForReport :: BaselineExperimentRuleCfg -> Int -> Action ()
dependOnBaselinePlotsForReport bac = dependOnPlotsForReport $ plotsForBaseline bac

baselineAnalysisRulesFor :: BaselineExperimentRuleCfg -> Rules (Maybe String)
baselineAnalysisRulesFor bac = do
    let results = csvOutFile bac
    onlyIfFileExists results $ do
        let plotsForThisBaseline = plotsForBaseline bac
        plotsForThisBaseline &%> \_ -> do
            needsToExist results
            need [baselineAnalysisScript]

            let prefix = baselinePrefixFor bac

            need [rBin]
            needRLibs ["pkgmaker"]
            needRLibs ["igraph"]
            unit $ rScript baselineAnalysisScript results prefix analysisPlotsDir

        let thisTarget = baselineAnalysisRuleFor bac
        thisTarget ~> need plotsForThisBaseline
        return thisTarget
