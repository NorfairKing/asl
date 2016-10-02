module AslBuild.Analysis where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Baseline
import           AslBuild.Baseline.Types
import           AslBuild.Constants


localResults :: FilePath
localResults = csvOutFile localBaselineExperiment

analysisDir :: FilePath
analysisDir = "analysis"

baselineLocalhostPrefix :: FilePath
baselineLocalhostPrefix = "baseline-localhost"

localhostPlotTps :: FilePath
localhostPlotTps = analysisDir </> (baselineLocalhostPrefix ++ "-tps") <.> pngExt

localhostPlotAvg :: FilePath
localhostPlotAvg = analysisDir </> (baselineLocalhostPrefix ++ "-avg") <.> pngExt

localhostPlots :: [FilePath]
localhostPlots = [localhostPlotTps, localhostPlotAvg]

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
        cmd rCmd analysisScript localResults localhostPlotTps localhostPlotAvg

    cleanAnalysisRule ~> removeFilesAfter analysisDir ["//*.png"]

