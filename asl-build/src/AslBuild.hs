module AslBuild
    ( aslBuild
    ) where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad.Reader

import           Data.List.Split
import           System.Environment         (getArgs, withArgs)

import           AslBuild.Constants
import           AslBuild.OptParse

aslBuild :: IO ()
aslBuild = do
    args <- getArgs
    let splitt = splitOn ["--"] args
    case splitt of
        [] -> putStrLn "Should never happen"
        (first:rests) -> do
            let rest = concat rests
            (Command, flags) <- getArguments first
            withArgs rest $ doTheShake flags

doTheShake :: Flags -> IO ()
doTheShake flags = shakeArgs shakeOptions $
    flip runReaderT flags $ do
    commitHashRules
    jarRules
    reportRules
    cleanRules

type AslBuilder = ReaderT Flags Rules

jarRules :: AslBuilder ()
jarRules = lift $ do
    let jarFile = asl <.> jar
        jarout = outDir </> jarFile
    want [jarout]

    let jarInGradleBuildDir = codeSrcDir </> build </> libs </> jarFile
    jarInGradleBuildDir %> \_ -> do
        let buildFile = codeSrcDir </> build <.> gradle
            settingsFile = codeSrcDir </> settings <.> gradle
        need [buildFile, settingsFile]
        sourceFiles <- absFilesInDir javaSourceDir ["//*" <.> java]
        need sourceFiles
        let jarTarget = jar
            gradleCmd = gradle
        cmd (Cwd codeSrcDir) gradleCmd jarTarget

    jarout `byCopying` jarInGradleBuildDir

absFilesInDir :: FilePath -> [FilePath] -> Action [FilePath]
absFilesInDir dir pats = map (dir </>) <$> getDirectoryFiles dir pats

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

byCopying :: FilePath -> FilePath -> Rules ()
byCopying to from = to %> \_ -> copyFileChanged from to

commitHashRules :: AslBuilder ()
commitHashRules = lift $ do
    let commithash = "commit"
        commithashFile = commithash <.> txtExt
    want [commithashFile]
    commithashFile %> \_ -> do
        alwaysRerun
        -- Make the hash as short as possible with --short
        Stdout hash <- quietly $ cmd "git rev-parse --short=0 --verify HEAD"
        Stdout dirtyStr <- quietly $ cmd "git status --porcelain"
        -- # init to remove newline
        let contents = init hash ++ if null (dirtyStr :: String) then [] else "-dirty"
        writeFileChanged commithashFile contents

cleanRules :: AslBuilder ()
cleanRules = lift $ phony "clean" $ do
    removeFilesAfter outDir ["//"]
    removeFilesAfter codeSrcDir ["//build", "//out"]
    removeFilesAfter reportsDir ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

