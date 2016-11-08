module AslBuild.Reports.MileStone1 where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Baseline
import           AslBuild.Analysis.StabilityTrace
import           AslBuild.Constants
import           AslBuild.Reports.Common

architecturePng :: FilePath
architecturePng = reportAssetsDir 1 </> "architecture" <.> pngExt

architectureGraphName :: String
architectureGraphName = "architecture_graph"

architectureGraphDot :: FilePath
architectureGraphDot = reportGraphsDir 1 </> architectureGraphName <.> dotExt

architectureGraphEps :: FilePath
architectureGraphEps = reportGraphsDir 1 </> architectureGraphName <.> epsExt

report1Rules :: Rules ()
report1Rules = report 1 texPreAction customRules
  where
    customRules =
        architectureGraphDot `compileDotToEps` architectureGraphEps

    texPreAction =
        need
            [ architecturePng
            , architectureGraphEps
            , baselineAnalysisRuleFor remoteBaselineAnalysis
            , stabilityTraceAnalysisRuleFor remoteStabilityTraceAnalysis
            ]
