module AslBuild.Reports where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis
import           AslBuild.Constants
import           AslBuild.Utils

milestone1Reportname :: String
milestone1Reportname = "r1"

milestone1ReportOut :: FilePath
milestone1ReportOut = outDir </> "tom_sydney_kerckhove_asl_report_milestone1" <.> pdfExt

milestone1ReportInBuildDir :: FilePath
milestone1ReportInBuildDir = reportsDir </> milestone1Reportname <.> pdfExt

milestone1Reporttex :: FilePath
milestone1Reporttex = milestone1Reportname  <.> texExt

milestone1ReporttexInBuildDir :: FilePath
milestone1ReporttexInBuildDir = reportsDir </> milestone1Reporttex

commonTex :: FilePath
commonTex = reportsDir </> "common" <.> texExt

reportsRule :: String
reportsRule = "reports"

cleanReportsRule :: String
cleanReportsRule = "clean-reports"

reportRules :: Rules ()
reportRules = do
    reportsRule ~> need [milestone1ReportOut]

    milestone1ReportInBuildDir %> \_ -> do
        need $ [commonTex, milestone1ReporttexInBuildDir] ++ plotsFor remoteBaselineAnalysis
        cmd (Cwd reportsDir) "latexmk" "-pdf" milestone1Reporttex

    milestone1ReportOut `byCopying` milestone1ReportInBuildDir

    phony cleanReportsRule $ do
        removeFilesAfter outDir [milestone1ReportOut]
        removeFilesAfter reportsDir
            ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

