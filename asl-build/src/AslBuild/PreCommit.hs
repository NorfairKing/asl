module AslBuild.PreCommit where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.OptParse
import           AslBuild.Test
import           AslBuild.Utils

scriptsLib :: FilePath
scriptsLib = scriptsDir </> "lib.sh"

linesScript :: FilePath
linesScript = scriptsDir </> "lines.sh"

preCommitRule :: String
preCommitRule = "pre-commit"

preCommitRules :: AslBuilder ()
preCommitRules = lift $ do
    preCommitRule ~> do
        need
            [ outputJarFile
            , buildBinOut
            , codeHealthRule
            , testRule
            , documentationRule
            , formatClientRule
            ]
        unit $ cmd gitCmd "add" "." -- Re-add files that were formatted.
        unit $ cmd linesScript

    buildBin
    codeHealth
    documentation
    formatClient

buildBinOut :: FilePath
buildBinOut = outDir </> asl

buildBinInStack :: FilePath
buildBinInStack = ".stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin/asl"

aslBuildDir :: FilePath
aslBuildDir = "asl-build"

buildBin :: Rules ()
buildBin = do
    buildBinInStack %> \_ -> do
        files <- getDirectoryFiles "" ["asl-build//*.hs"]
        need files
        cmd (Cwd aslBuildDir) stackCmd "build"

    buildBinOut `byCopying` buildBinInStack


codeHealthRule :: String
codeHealthRule = "code-health"

codeHealth :: Rules ()
codeHealth = do
    codeHealthRule ~> need
        [ trailingWhitespaceRule
        , indentationRule
        , hlintRule
        , sanityRule
        ]
    trailingWhitespace
    indentation
    hlint
    sanity

trailingWhitespaceRule :: String
trailingWhitespaceRule = "trailing-whitespace"

trailingWhitespaceScript :: String
trailingWhitespaceScript = scriptsDir </> "trailing_whitespace_test.sh"

trailingWhitespace :: Rules ()
trailingWhitespace = trailingWhitespaceRule ~> do
    need [scriptsLib, trailingWhitespaceScript]
    cmd trailingWhitespaceScript

indentationRule :: String
indentationRule = "indentation"

indentationScript :: FilePath
indentationScript = scriptsDir </> "indentation.sh"

indentation :: Rules ()
indentation = indentationRule ~> do
    need [scriptsLib, indentationScript]
    cmd indentationScript

hlintRule :: String
hlintRule = "hlint"

hlintScript :: FilePath
hlintScript = scriptsDir </> "hlint_health.sh"

hlint :: Rules ()
hlint = hlintRule ~> do
    need [scriptsLib, hlintScript]
    cmd hlintScript

sanityRule :: String
sanityRule = "sanity"

sanity :: Rules ()
sanity = sanityRule ~> sanityIn aslBuildDir

sanityIn :: FilePath -> Action ()
sanityIn dir = do
    unit $ cmd (Cwd dir) stackCmd "clean"
    let ghcOpts =
            [ "-Wall"
            , "-Werror"
            , "-fwarn-unused-imports"
            , "-fwarn-incomplete-patterns"
            , "-fwarn-unused-do-bind"
            , "-fno-warn-orphans"
            ]

    cmd (Cwd dir)
        stackCmd "build"
        "--fast"
        "--ghc-options" [unwords ghcOpts]


documentationRule :: String
documentationRule = "documentation"

documentation :: Rules ()
documentation = documentationRule ~> cmd stackCmd "haddock"

javaFormatterUrl :: FilePath
javaFormatterUrl = "https://github.com/google/google-java-format/releases/download/google-java-format-1.0/google-java-format-1.0-all-deps.jar"

javaFormatter :: FilePath
javaFormatter = tmpDir </> "google-java-format-1.0-all-deps.jar"

formatClientRule :: String
formatClientRule = "format-client"

formatClient :: Rules ()
formatClient = do
    formatClientRule ~> do
        files <- getDirectoryFiles "" ["inbox-client//*.java"]
        need files
        void $ forP files javaFormat

    javaFormatter %> \_ ->
        cmd wgetCmd javaFormatterUrl "--output-document" javaFormatter

javaFormat :: FilePath -> Action ()
javaFormat abspath = do
    need [javaFormatter]
    Stdout formattedSrc <- cmd javaCmd "-jar" javaFormatter abspath
    writeFileChanged abspath formattedSrc

