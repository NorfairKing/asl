module AslBuild.Reports where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Baseline
import           AslBuild.Analysis.StabilityTrace
import           AslBuild.Constants
import           AslBuild.Utils

commonTex :: FilePath
commonTex = reportsDir </> "common" <.> texExt

reportsRule :: String
reportsRule = "reports"

cleanReportsRule :: String
cleanReportsRule = "clean-reports"

reportRules :: Rules ()
reportRules = do
    reportsRule ~> need [report1Rule]

    phony cleanReportsRule $ do
        removeFilesAfter outDir [milestone1ReportOut]
        removeFilesAfter reportsDir
            ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

    report1Rules

report1Rule :: String
report1Rule = "report1"

milestone1Reportname :: String
milestone1Reportname = "r1"

milestone1ReportOut :: FilePath
milestone1ReportOut = aslDir </> "tomk-milestone1" <.> pdfExt

milestone1ReportInBuildDir :: FilePath
milestone1ReportInBuildDir = report1Dir </> milestone1Reportname <.> pdfExt

milestone1Reporttex :: FilePath
milestone1Reporttex = milestone1Reportname  <.> texExt

milestone1Reportbib :: FilePath
milestone1Reportbib = milestone1Reportname  <.> "bib"

milestone1ReporttexInBuildDir :: FilePath
milestone1ReporttexInBuildDir = report1Dir </> milestone1Reporttex

milestone1ReportbibInBuildDir :: FilePath
milestone1ReportbibInBuildDir = report1Dir </> milestone1Reportbib

report1AssetsDir :: FilePath
report1AssetsDir = report1Dir </> "assets"

architecturePng :: FilePath
architecturePng = report1AssetsDir </> "architecture" <.> pngExt

report1Graphs :: FilePath
report1Graphs = report1Dir </> "graphs"

architectureGraphName :: String
architectureGraphName = "architecture_graph"

architectureGraphDot :: FilePath
architectureGraphDot = report1Graphs </> architectureGraphName <.> dotExt

architectureGraphEps :: FilePath
architectureGraphEps = report1Graphs </> architectureGraphName <.> epsExt

report1Rules :: Rules ()
report1Rules = do
    report1Rule ~> need [milestone1ReportOut]

    milestone1ReportInBuildDir %> \_ -> do
        need $
            [commonTex, architecturePng, architectureGraphEps, milestone1ReporttexInBuildDir, milestone1ReportbibInBuildDir]
            ++ plotsForBaseline remoteBaselineAnalysis
            ++ plotsForStabilityTrace localStabilityTraceAnalysis
        cmd (Cwd report1Dir)
            "latexmk"
            milestone1Reporttex
            "-pdf"
            "-interaction=nonstopmode"
            "-shell-escape"

    architectureGraphEps %> \_ -> do
        need [architectureGraphDot]
        cmd (FileStdout architectureGraphEps) dotCmd "-Teps" architectureGraphDot

    milestone1ReportOut `byCopying` milestone1ReportInBuildDir
