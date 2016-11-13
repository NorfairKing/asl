module AslBuild.Analysis where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.MaximumThroughput
import           AslBuild.Analysis.StabilityTrace
import           AslBuild.Analysis.TraceSlice
import           AslBuild.Constants

analysisScript :: FilePath
analysisScript = analysisDir </> "analyze.r"

analysisRule :: String
analysisRule = "analysis"

cleanAnalysisRule :: String
cleanAnalysisRule = "clean-analysis"

analysisRules :: Rules ()
analysisRules = do
    buildRRules

    stabilityTraceAnalysisRules
    traceSliceAnalysisRules
    throughputAnalysisRules

    analysisRule ~> need
        [ stabilityTraceAnalysisRule
        , traceSliceAnalysisRule
        , throughputAnalysisRule
        ]

    cleanAnalysisRule ~> removeFilesAfter analysisDir ["//*.png"]
