{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis where

import           Data.List

import           Development.Shake
import           Development.Shake.Command
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

    mapM_ baselineAnalysisRuleFor allBaselineAnalyses

    mapM_ (uncurry rlib) rdeps

    cleanAnalysisRule ~> removeFilesAfter analysisDir ["//*.png"]

baselineAnalysisRuleFor :: BaselineAnalysisCfg -> Rules ()
baselineAnalysisRuleFor bac@BaselineAnalysisCfg{..} = plotsFor bac &%> \_ -> do
    let results = csvOutFile experiment
    resultsExist <- doesFileExist results

    need $ analysisScript : [results | not resultsExist] -- Do not depend on results if they exist already.

    needRLib "igraph"
    unit $ rScript analysisScript results $ analysisDir </> filePrefix

rScript :: CmdArguments args => args
rScript = cmd rCmd (AddEnv "R_LIBS" rlibdir)

rlib :: String -> String -> Rules ()
rlib name url = do
    let archiveFile = rlibdir </> name <.> tarGzExt
    archiveFile %> \_ -> cmd curlCmd "--output" archiveFile url
    rLibTarget name %> \_ -> do
        need [archiveFile]
        cmd "R" "CMD" "INSTALL" "-l" rlibdir archiveFile

rLibTarget :: String -> FilePath
rLibTarget name = rlibdir </> name </> "R" </> name

needRLib :: String -> Action ()
needRLib name = need [rLibTarget name]

rdeps :: [(String, String)]
rdeps =
    [ ("igraph", "https://cran.r-project.org/src/contrib/igraph_1.0.1.tar.gz")
    ]
