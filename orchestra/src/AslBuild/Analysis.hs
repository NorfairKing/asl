module AslBuild.Analysis where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.MaximumThroughput
import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.ReplicationEffect
import           AslBuild.Analysis.StabilityTrace
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.TraceSlice
import           AslBuild.Analysis.WriteEffect
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

    memaslapLogsRules
    traceRules

    stabilityTraceAnalysisRules
    traceSliceAnalysisRules
    throughputAnalysisRules
    replicationAnalysisRules
    writeAnalysisRules

    analysisRule ~> need
        [ stabilityTraceAnalysisRule
        , traceSliceAnalysisRule
        , throughputAnalysisRule
        , replicationAnalysisRule
        , writeAnalysisRule
        ]

    cleanAnalysisRule ~> removeFilesAfter analysisDir ["//*.png"]
