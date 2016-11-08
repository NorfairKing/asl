module AslBuild.Reports.Common where

import           Control.Monad

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Utils

commonTex :: FilePath
commonTex = reportsDir </> "common" <.> texExt

buildLatexIn :: FilePath -> FilePath -> Action ()
buildLatexIn texFile texDir =
    cmd (Cwd texDir)
        "latexmk"
        texFile
        "-pdf"
        "-interaction=nonstopmode"
        "-shell-escape"

compileDotToEps :: FilePath -> FilePath -> Rules ()
compileDotToEps dotFile epsFile = epsFile %> \_ -> do
    need [dotFile]
    cmd (FileStdout epsFile) dotCmd "-Teps" dotFile

reportRule :: Int -> String
reportRule i = "report" ++ show i

reportCleanRule :: Int -> String
reportCleanRule i = "clean-report" ++ show i

reportOut :: Int -> FilePath
reportOut i = aslDir </> myNetzh ++ "-milestone" ++ show i <.> pdfExt

reportDir :: Int -> FilePath
reportDir i = reportsDir </> "report" ++ show i

reportName :: Int -> FilePath
reportName i = "r" ++ show i

reportInBuildDir :: Int -> FilePath
reportInBuildDir i = reportDir i </> reportName i <.> pdfExt

reportTex :: Int -> FilePath
reportTex i = reportName i <.> texExt

reportBib :: Int -> FilePath
reportBib i = reportName i <.> bibExt

reportTexInBuildDir :: Int -> FilePath
reportTexInBuildDir i = reportDir i </> reportTex i

reportBibInBuildDir :: Int -> FilePath
reportBibInBuildDir i = reportDir i </> reportBib i

reportAssetsDir :: Int -> FilePath
reportAssetsDir i = reportDir i </> "assets"

reportGraphsDir :: Int -> FilePath
reportGraphsDir i = reportDir i </> "graphs"

reportPlotsDir :: Int -> FilePath
reportPlotsDir i = reportDir i </> "plots"

plotForReport :: FilePath -> Int -> FilePath
plotForReport plot i = plot `replaceDirectory` reportPlotsDir i

usePlotInReport :: FilePath -> Int -> Rules ()
usePlotInReport plot i = (plot `plotForReport` i) `byCopying` plot

usePlotsInReport :: [FilePath] -> Int -> Rules ()
usePlotsInReport plots i = forM_ plots (`usePlotInReport` i)

dependOnPlotsForReport :: [FilePath] -> Int -> Action ()
dependOnPlotsForReport plots i = need $ map (`plotForReport` i) plots

report
    :: Int -- The milestone number
    -> Action () -- The action to do everything that needs to be done BEFORE building the report
    -> Rules () -- Custom rules
    -> Rules ()
report i texPreAction customRules = do
    reportRule i ~> need [reportOut i]

    customRules

    reportOut i `byCopying` reportInBuildDir i

    reportInBuildDir i %> \_ -> do
        need [commonTex, reportTexInBuildDir i, reportBibInBuildDir i]

        texPreAction

        reportTex i`buildLatexIn` reportDir i

    reportCleanRule i ~> do
        removeFilesAfter "" [reportOut i]
        removeFilesAfter (reportDir i)
            ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]
