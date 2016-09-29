module AslBuild.Reports where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

cleanReportsRule :: String
cleanReportsRule = "cleanreports"

reportRules :: AslBuilder ()
reportRules = do
    c <- ask
    lift $ do
        let milestone1Reportname = "r1"
        let milestone1ReportOut = outDir </> "tom_sydney_kerckhove_asl_report_milestone1" <.> pdfExt
        let milestone1ReportInBuildDir = reportsDir </> milestone1Reportname <.> pdfExt
        let milestone1Reporttex = milestone1Reportname  <.> texExt
        let milestone1ReporttexInBuildDir = reportsDir </> milestone1Reporttex
        let commonTex = reportsDir </> "common" <.> texExt
        case c of
            BuildAll -> want [milestone1ReportOut]
            BuildReports -> want [milestone1ReportOut]
            BuildClean -> want [cleanReportsRule]
            _ -> return ()

        milestone1ReportInBuildDir %> \_ -> do
            need [commonTex]
            need [milestone1ReporttexInBuildDir]
            cmd (Cwd reportsDir) "latexmk" "-pdf" milestone1Reporttex

        milestone1ReportOut `byCopying` milestone1ReportInBuildDir

        phony cleanReportsRule $ do
            removeFilesAfter outDir [milestone1ReportOut]
            removeFilesAfter reportsDir
                ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

