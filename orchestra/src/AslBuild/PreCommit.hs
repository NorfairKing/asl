module AslBuild.PreCommit where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis
import           AslBuild.BuildMemcached
import           AslBuild.Constants
import           AslBuild.Experiments.Baseline
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.LocalMiddlewareTest.MultiClientTest
import           AslBuild.LocalMiddlewareTest.MultipleClientsTest
import           AslBuild.LocalMiddlewareTest.MultipleServersTest
import           AslBuild.LocalMiddlewareTest.ParseTest
import           AslBuild.LocalMiddlewareTest.ReplicationTest
import           AslBuild.LocalMiddlewareTest.SimpleTest
import           AslBuild.Orc
import           AslBuild.Reports
import           AslBuild.Test
import           AslBuild.Utils

scriptsLib :: FilePath
scriptsLib = scriptsDir </> "lib.sh"

preCommitRule :: String
preCommitRule = "pre-commit"

preCommitRules :: Rules ()
preCommitRules = do
    preCommitRule ~> do
        need [outputJarFile]
        need [orcBin]
        need [memcachedBin]
        need [memaslapBin]

        need [codeHealthRule]
        need [testRule]
        need [documentationRule]

        need [localLogTestRule]
        need [localMiddlewareParseTestRule]
        need [localMiddlewareMultiClientTestRule]
        need [localMiddlewareReplicationTestRule]
        need [localMiddlewareSimpleTestRule]
        need [localMiddlewareMultipleServersTestRule]
        need [localMiddlewareMultipleClientsTestRule]

        need [smallLocalBaselineExperimentRule]
        need [smallLocalStabilityTraceRule]
        need [smallLocalMaximumThroughputRule]
        need [smallLocalReplicationEffectRule]
        need [smallLocalWriteEffectRule]

        need [analysisRule]
        need [reportsRule]

        need [formatClientRule]
        unit $ cmd (Cwd aslDir) gitCmd "add" "." -- Re-add files that were formatted.

        unit $ cmd (Cwd aslDir) "scripts/lines.sh"

    codeHealth
    documentation
    formatClient

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
        forP_ files javaFormat

    javaFormatter %> \_ ->
        cmd wgetCmd javaFormatterUrl "--output-document" javaFormatter

javaFormat :: FilePath -> Action ()
javaFormat abspath = do
    need [javaFormatter]
    Stdout formattedSrc <- cmd javaCmd "-jar" javaFormatter abspath
    writeFileChanged abspath formattedSrc

