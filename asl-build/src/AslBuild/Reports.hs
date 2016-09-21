module AslBuild.Reports where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

reportRules :: AslBuilder ()
reportRules = do
    usingTravis <- asks flagsTravis
    unless usingTravis $ lift $ do
        let reportstubname = "reportstub"
        let reportStubOut = outDir </> reportstubname <.> pdfExt
        let reportStubInBuildDir = reportsDir </> reportstubname <.> pdfExt
        let reportstubtex = reportstubname  <.> texExt
        let reportstubtexInBuildDir = reportsDir </> reportstubtex
        want [reportStubOut]
        reportStubInBuildDir %> \_ -> do
            need [reportstubtexInBuildDir]
            cmd (Cwd reportsDir) "latexmk" "-pdf" reportstubtex
        reportStubOut `byCopying` reportStubInBuildDir
