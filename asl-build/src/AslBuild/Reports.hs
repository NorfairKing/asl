module AslBuild.Reports where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

reportRules :: AslBuilder ()
reportRules = do
    c <- ask
    lift $ do
        let reportstubname = "reportstub"
        let reportStubOut = outDir </> reportstubname <.> pdfExt
        let reportStubInBuildDir = reportsDir </> reportstubname <.> pdfExt
        let reportstubtex = reportstubname  <.> texExt
        let reportstubtexInBuildDir = reportsDir </> reportstubtex
        case c of
            BuildAll onTravis -> unless onTravis $ want [reportStubOut]
            BuildReports -> want [reportStubOut]
            BuildClean -> want ["cleanreports"]
            _ -> return ()

        reportStubInBuildDir %> \_ -> do
            need [reportstubtexInBuildDir]
            cmd (Cwd reportsDir) "latexmk" "-pdf" reportstubtex

        reportStubOut `byCopying` reportStubInBuildDir

        phony "cleanreports" $ do
            removeFilesAfter outDir ["//"]
            removeFilesAfter reportsDir
                ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

