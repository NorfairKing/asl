module AslBuild.Analysis where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Baseline
import           AslBuild.Baseline.Types
import           AslBuild.Constants

experiment :: BaselineExperimentRuleCfg
experiment = localBaselineExperiment -- bigLocalBaselineExperiment

localResults :: FilePath
localResults = csvOutFile experiment

analysisDir :: FilePath
analysisDir = "analysis"

baselineLocalhostPrefix :: FilePath
baselineLocalhostPrefix = "baseline-localhost"

localhostPlotTpsFilePrefix :: FilePath
localhostPlotTpsFilePrefix = analysisDir </> (baselineLocalhostPrefix ++ "-tps")

localhostPlotAvgFilePrefix :: FilePath
localhostPlotAvgFilePrefix = analysisDir </> (baselineLocalhostPrefix ++ "-avg")

localhostPlots :: [FilePath]
localhostPlots = do
    prefix <- [localhostPlotTpsFilePrefix, localhostPlotAvgFilePrefix]
    nrclients <- [1 .. maxNrClients experiment]
    return $ prefix ++ "-" ++ show nrclients <.> pngExt

analysisScript :: FilePath
analysisScript = analysisDir </> "analyze.r"

analysisRule :: String
analysisRule = "analysis"

cleanAnalysisRule :: String
cleanAnalysisRule = "clean-analysis"

analysisRules :: Rules ()
analysisRules = do
    analysisRule ~> need localhostPlots

    localhostPlots &%> \_ -> do
        need [analysisScript]--, localResults]
        cmd rCmd analysisScript localResults localhostPlotTpsFilePrefix localhostPlotAvgFilePrefix

    cleanAnalysisRule ~> removeFilesAfter analysisDir ["//*.png"]

