{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis where

import           Data.List

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath

import           AslBuild.Baseline
import           AslBuild.Baseline.Types
import           AslBuild.Constants
import           AslBuild.Utils

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
    rRules
    mapM_ rlib rLibs

    analysisRule ~> need allPlots

    mapM_ baselineAnalysisRuleFor allBaselineAnalyses

    cleanAnalysisRule ~> removeFilesAfter analysisDir ["//*.png"]

baselineAnalysisRuleFor :: BaselineAnalysisCfg -> Rules ()
baselineAnalysisRuleFor bac@BaselineAnalysisCfg{..} = plotsFor bac &%> \_ -> do
    let results = csvOutFile experiment
    resultsExist <- doesFileExist results

    need $ analysisScript : [results | not resultsExist] -- Do not depend on results if they exist already.

    need [rBin]
    needRLibs ["pkgmaker"]
    needRLibs ["igraph"]
    unit $ rScript analysisScript results $ analysisDir </> filePrefix

rScript :: CmdArguments args => args
rScript = cmd rBin (AddEnv "R_LIBS" rlibdir)

rlib :: String -> Rules ()
rlib name =
    rLibTarget name %> \_ -> do
        need [rBin]
        let tmpInstallScript = "/tmp/install-" ++ name <.> "r"
        writeFile' tmpInstallScript $ unlines
            [ "repos <- \"http://cran.rstudio.com\""
            , "libloc <- \"" ++ rlibdir ++ "\""
            , "update.packages(repos=repos, ask=FALSE, lib=libloc)"
            , "install.packages(c(\"" ++ name ++ "\"), repos=repos, lib=libloc)"
            ]
        cmd rBin tmpInstallScript

rLibTarget :: String -> FilePath
rLibTarget name = rlibdir </> name </> "R" </> name

needRLibs :: [String] -> Action ()
needRLibs names = need $ map rLibTarget names

rLibs :: [String]
rLibs =
    [ "igraph"
    , "pkgmaker"
    ]


rArchive :: FilePath
rArchive = tmpDir </> "R.tar.gz"

rVersion :: String
rVersion = "R-3.1.0"

rLink :: FilePath
rLink = "https://cran.r-project.org/src/base/R-3" </> rVersion <.> tarGzExt

rDir :: FilePath
rDir = tmpDir </> "R"

rBin :: FilePath
rBin = outDir </> "Rscript"

rInstallBin :: FilePath
rInstallBin = outDir </> "R"

rMakeDir :: FilePath
rMakeDir = rDir </> rVersion

rConfigureScriptName :: FilePath
rConfigureScriptName = "configure"

rConfigureScript :: FilePath
rConfigureScript = rMakeDir </> rConfigureScriptName

rMakefile :: FilePath
rMakefile = rMakeDir </> "Makefile"

rBinInMakeDir :: FilePath
rBinInMakeDir = rMakeDir </> "bin" </> "Rscript"

rInstallBinInMakeDir :: FilePath
rInstallBinInMakeDir = rMakeDir </> "bin" </> "R"

rlibdir :: FilePath
rlibdir = rMakeDir </> "library"

rRules :: Rules ()
rRules = do
    want [rBin]
    rArchive %> \_ ->
        cmd curlCmd
            "--output" rArchive
            rLink

    rConfigureScript %> \_ -> do
        need [rArchive]
        cmd tarCmd
            "--extract"
            "--verbose"
            "--file" rArchive
            "--directory" rDir

    rMakefile %> \_ -> do
        need [rConfigureScript]
        cmd (Cwd rMakeDir) ("." </> rConfigureScriptName)

    [rBinInMakeDir, rInstallBinInMakeDir] &%> \_ -> do
        need [rMakefile]
        cmd (Cwd rMakeDir) "make" "--jobs"

    rBin `byCopying` rBinInMakeDir
    rInstallBin `byCopying` rInstallBinInMakeDir
