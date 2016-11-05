module AslBuild.Analysis where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Baseline
import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.StabilityTrace
import           AslBuild.Analysis.TraceSlice
import           AslBuild.Constants

analysisScript :: FilePath
analysisScript = analysisDir </> "analyze.r"

analysisRule :: String
analysisRule = "analysis"

cleanAnalysisRule :: String
cleanAnalysisRule = "clean-analysis"

allPlots :: [FilePath]
allPlots = allBaselinePlots ++ allStabilityTracePlots ++ allTraceSlicePlots

analysisRules :: Rules ()
analysisRules = do
    buildRRules

    baselineAnalysisRules
    stabilityTraceAnalysisRules
    traceSliceAnalysisRules

    analysisRule ~> need allPlots
    cleanAnalysisRule ~> removeFilesAfter analysisDir ["//*.png"]
