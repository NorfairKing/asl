{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis where

import           Data.List

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Baseline
import           AslBuild.Baseline.Types
import           AslBuild.Constants

analysisScript :: FilePath
analysisScript = analysisDir </> "analyze.r"

analysisRule :: String
analysisRule = "analysis"

cleanAnalysisRule :: String
cleanAnalysisRule = "clean-analysis"

data BaselineAnalysisCfg
    = BaselineAnalysisCfg
    { experiment :: BaselineExperimentRuleCfg
    , filePrefix :: FilePath
    }

plotsFor :: BaselineAnalysisCfg -> [FilePath]
plotsFor BaselineAnalysisCfg{..} = do
    prefix <- ["avg", "tps"]
    nrclients <- [1 .. maxNrClients experiment]
    return $ analysisDir </> intercalate "-" [filePrefix, prefix, show nrclients] <.> pngExt

remoteBaselineAnalysis :: BaselineAnalysisCfg
remoteBaselineAnalysis = BaselineAnalysisCfg
    { experiment = remoteBaselineExperiment
    , filePrefix = "remote-baseline-experiment"
    }

smallLocalBaselineAnalysis :: BaselineAnalysisCfg
smallLocalBaselineAnalysis = BaselineAnalysisCfg
    { experiment = smallLocalBaselineExperiment
    , filePrefix = "small-local-baseline-experiment"
    }

localBaselineAnalysis :: BaselineAnalysisCfg
localBaselineAnalysis = BaselineAnalysisCfg
    { experiment = localBaselineExperiment
    , filePrefix = "local-baseline-experiment"
    }

bigLocalBaselineAnalysis :: BaselineAnalysisCfg
bigLocalBaselineAnalysis = BaselineAnalysisCfg
    { experiment = bigLocalBaselineExperiment
    , filePrefix = "big-local-baseline-experiment"
    }

allBaselineAnalyses :: [BaselineAnalysisCfg]
allBaselineAnalyses =
    [ smallLocalBaselineAnalysis
    , localBaselineAnalysis
    , bigLocalBaselineAnalysis
    , remoteBaselineAnalysis
    ]

allPlots :: [FilePath]
allPlots = concatMap plotsFor allBaselineAnalyses

analysisRules :: Rules ()
analysisRules = do
    analysisRule ~> need allPlots

    mapM_ analysisRuleFor allBaselineAnalyses

    cleanAnalysisRule ~> removeFilesAfter analysisDir ["//*.png"]

analysisRuleFor :: BaselineAnalysisCfg -> Rules ()
analysisRuleFor bac@BaselineAnalysisCfg{..} = plotsFor bac &%> \_ -> do
    let results = csvOutFile experiment
    resultsExist <- doesFileExist results
    need $ analysisScript : [results | not resultsExist] -- Do not depend on results if they exist already.
    unit $ cmd rCmd analysisScript results $ analysisDir </> filePrefix

