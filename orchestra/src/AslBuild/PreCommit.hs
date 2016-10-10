module AslBuild.PreCommit where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.LocalMiddlewareMultipleServersTest
import           AslBuild.LocalMiddlewareParseTest
import           AslBuild.LocalMiddlewareTest
import           AslBuild.OptParse
import           AslBuild.Test

scriptsLib :: FilePath
scriptsLib = scriptsDir </> "lib.sh"

preCommitRule :: String
preCommitRule = "pre-commit"

preCommitRules :: Rules ()
preCommitRules = do
    preCommitRule ~> do
        need
            [ outputJarFile
            , buildBinInStack
            , codeHealthRule
            , testRule
            , documentationRule
            , formatClientRule
            ]
        unit $ cmd (Cwd aslDir) gitCmd "add" "." -- Re-add files that were formatted.

        need
            [ localLogTestRule
            , localMiddlewareParseTestRule
            , localMiddlewareTestRule
            , localMiddlewareMultipleServersTestRule
            ]
        unit $ cmd (Cwd aslDir) "scripts/lines.sh"

    buildBin
    codeHealth
    documentation
    formatClient

buildBinInStack :: FilePath
buildBinInStack = aslDir </> ".stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin/orc"

aslBuildDir :: FilePath
aslBuildDir = aslDir </> "orchestra"

buildBin :: Rules ()
buildBin =
    buildBinInStack %> \_ -> do
        files <- getDirectoryFiles "" [aslBuildDir <//> "*.hs"]
        need files
        cmd (Cwd aslBuildDir) stackCmd "build"

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
    cmd (Cwd aslDir) "scripts/trailing_whitespace_test.sh"

indentationRule :: String
indentationRule = "indentation"

indentationScript :: FilePath
indentationScript = scriptsDir </> "indentation.sh"

indentation :: Rules ()
indentation = indentationRule ~> do
    need [scriptsLib, indentationScript]
    cmd (Cwd aslDir) "scripts/indentation.sh"

hlintRule :: String
hlintRule = "hlint"

hlintScript :: FilePath
hlintScript = scriptsDir </> "hlint_health.sh"

hlint :: Rules ()
hlint = hlintRule ~> do
    need [scriptsLib, hlintScript]
    cmd (Cwd aslDir) "scripts/hlint_health.sh"

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
documentation = documentationRule ~> cmd (Cwd aslDir) stackCmd "haddock"

javaFormatterUrl :: FilePath
javaFormatterUrl = "https://github.com/google/google-java-format/releases/download/google-java-format-1.0/google-java-format-1.0-all-deps.jar"

javaFormatter :: FilePath
javaFormatter = tmpDir </> "google-java-format-1.0-all-deps.jar"

formatClientRule :: String
formatClientRule = "format-client"

formatClient :: Rules ()
formatClient = do
    formatClientRule ~> do
        srcFiles <- getDirectoryFiles "" [javaSourceDir <//> "*" <.> javaExt]
        testFiles <- getDirectoryFiles "" [javaTestDir <//> "*" <.> javaExt]
        let files = srcFiles ++ testFiles
        need files
        void $ forP files javaFormat

    javaFormatter %> \_ ->
        cmd wgetCmd javaFormatterUrl "--output-document" javaFormatter

javaFormat :: FilePath -> Action ()
javaFormat abspath = do
    need [javaFormatter]
    Stdout formattedSrc <- cmd javaCmd "-jar" javaFormatter abspath
    writeFileChanged abspath formattedSrc

