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

-- Directories
outDir :: FilePath
outDir = "out"

reportsDir :: FilePath
reportsDir = "reports"

codeSrcDir :: FilePath
codeSrcDir = "asl"

javaSourceDir :: FilePath
javaSourceDir = codeSrcDir </> "src"

-- Extensions
type Extension = FilePath

pdf :: Extension
pdf = "pdf"

tex :: Extension
tex = "tex"

jar :: Extension
jar = "jar"

java :: Extension
java = "java"

gradle :: Extension
gradle = "gradle"

doTheShake :: Flags -> IO ()
doTheShake flags = do
    shakeArgs shakeOptions $
        flip runReaderT flags $ do
        jarRules
        reportRules

type AslBuilder = ReaderT Flags Rules

jarRules :: AslBuilder ()
jarRules = lift $ do
    let jarFile = "asl" <.> jar
        jarout = outDir </> jarFile
    want [jarout]

    let jarInGradleBuildDir = codeSrcDir </> "build/libs" </> jarFile
    jarInGradleBuildDir %> \out -> do
        let buildFile = "build" <.> gradle
            settingsFile = "settings" <.> gradle
        need [buildFile, settingsFile]
        sourceFiles <- absFilesInDir javaSourceDir ["//*" <.> java]
        need sourceFiles
        command [Cwd codeSrcDir] "gradle" ["--build-file", buildFile, "jar"]

    jarout `byCopying` jarInGradleBuildDir

absFilesInDir :: FilePath -> [FilePath] -> Action [FilePath]
absFilesInDir dir pats = map (dir </>) <$> getDirectoryFiles dir pats

reportRules :: AslBuilder ()
reportRules = do
    usingTravis <- asks flagsTravis
    unless usingTravis $ lift $ do
        let reportstubname = "reportstub"
        let reportStubOut = outDir </> reportstubname <.> pdf
        let reportStubInBuildDir = reportsDir </> reportstubname <.> pdf
        let reportstubtex = reportsDir </> reportstubname  <.> tex
        want [reportStubOut]
        reportStubInBuildDir %> \out -> do
            need [reportstubtex]
            command [Cwd reportsDir] "latexmk" ["-pdf", reportstubtex]
        reportStubOut `byCopying` reportStubInBuildDir

byCopying :: FilePath -> FilePath -> Rules ()
byCopying to from = to %> \_ -> do
    need [from]
    cmd "cp" from to

