module AslBuild
    ( aslBuild
    ) where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad.Reader

import           AslBuild.OptParse

import           Data.List.Split
import           System.Environment         (getArgs, withArgs)

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

-- Constants
asl :: String
asl = "asl"

out :: String
out = "out"

src :: String
src = "src"

txt :: String
txt = "txt"

reports :: String
reports = "reports"

pdf :: String
pdf = "pdf"

tex :: String
tex = "tex"

jar :: String
jar = "jar"

java :: String
java = "java"

gradle :: String
gradle = "gradle"

build :: String
build = "build"

libs :: String
libs = "libs"

settings :: String
settings = "settings"

-- Directories
outDir :: FilePath
outDir = out

reportsDir :: FilePath
reportsDir = reports

codeSrcDir :: FilePath
codeSrcDir = asl

javaSourceDir :: FilePath
javaSourceDir = codeSrcDir </> src

-- Extensions
type Extension = FilePath

pdfExt :: Extension
pdfExt = pdf

texExt :: Extension
texExt = tex

txtExt :: Extension
txtExt = txt

jarExt :: Extension
jarExt = jar

javaExt :: Extension
javaExt = java

gradleExt :: Extension
gradleExt = gradle

doTheShake :: Flags -> IO ()
doTheShake flags = do
    shakeArgs shakeOptions $
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
    jarInGradleBuildDir %> \out -> do
        let buildFile = codeSrcDir </> build <.> gradle
            settingsFile = codeSrcDir </> settings <.> gradle
        need [buildFile, settingsFile]
        sourceFiles <- absFilesInDir javaSourceDir ["//*" <.> java]
        need sourceFiles
        let jarTarget = jar
            gradleCmd = gradle
        cmd (Cwd codeSrcDir) gradle jarTarget

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
        reportStubInBuildDir %> \out -> do
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
    commithashFile %> \out -> do
        alwaysRerun
        -- Make the hash as short as possible with --short
        Stdout hash <- quietly $ cmd "git rev-parse --short=0 --verify HEAD"
        Stdout dirtyStr <- quietly $ cmd "git ls-files --exclude-standard --others"
        -- # init to remove newline
        let contents = init hash ++ if null (dirtyStr :: String) then [] else "-dirty" ++ "\n"
        writeFileChanged out contents

cleanRules :: AslBuilder ()
cleanRules = lift $ phony "clean" $ do
    removeFilesAfter outDir ["//"]
    removeFilesAfter codeSrcDir ["//build", "//out"]
    removeFilesAfter reportsDir ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

