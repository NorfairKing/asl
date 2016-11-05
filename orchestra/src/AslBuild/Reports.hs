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
    reportsRule ~> need [report1Rule, report2Rule]

    phony cleanReportsRule $ do
        removeFilesAfter outDir [milestone1ReportOut, milestone2ReportOut]
        removeFilesAfter reportsDir
            ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

    report1Rules
    report2Rules

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
        need
            [ commonTex
            , architecturePng
            , architectureGraphEps
            , milestone1ReporttexInBuildDir
            , milestone1ReportbibInBuildDir
            , baselineAnalysisRuleFor remoteBaselineAnalysis
            , stabilityTraceAnalysisRuleFor remoteStabilityTraceAnalysis
            ]
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

report2Rule :: String
report2Rule = "report2"

milestone2Reportname :: String
milestone2Reportname = "r2"

milestone2ReportOut :: FilePath
milestone2ReportOut = aslDir </> "tomk-milestone2" <.> pdfExt

milestone2ReportInBuildDir :: FilePath
milestone2ReportInBuildDir = report2Dir </> milestone2Reportname <.> pdfExt

milestone2Reporttex :: FilePath
milestone2Reporttex = milestone2Reportname  <.> texExt

milestone2Reportbib :: FilePath
milestone2Reportbib = milestone2Reportname  <.> "bib"

milestone2ReporttexInBuildDir :: FilePath
milestone2ReporttexInBuildDir = report2Dir </> milestone2Reporttex

milestone2ReportbibInBuildDir :: FilePath
milestone2ReportbibInBuildDir = report2Dir </> milestone2Reportbib

report2AssetsDir :: FilePath
report2AssetsDir = report2Dir </> "assets"

report2Graphs :: FilePath
report2Graphs = report2Dir </> "graphs"

report2Rules :: Rules ()
report2Rules = do
    report2Rule ~> need [milestone2ReportInBuildDir, milestone2ReportOut]

    milestone2ReportInBuildDir %> \_ -> do
        need
            [ commonTex
            , milestone2ReporttexInBuildDir
            , milestone2ReportbibInBuildDir
            ]
        cmd (Cwd report2Dir)
            "latexmk"
            milestone2Reporttex
            "-pdf"
            "-interaction=nonstopmode"
            "-shell-escape"

    architectureGraphEps %> \_ -> do
        need [architectureGraphDot]
        cmd (FileStdout architectureGraphEps) dotCmd "-Teps" architectureGraphDot

    milestone2ReportOut `byCopying` milestone2ReportInBuildDir
