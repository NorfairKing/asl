module AslBuild.PreCommit where

import System.Exit

import Development.Shake
import Development.Shake.FilePath

import AslBuild.Analysis
import AslBuild.BuildMemcached
import AslBuild.Constants
import AslBuild.Experiment
import AslBuild.Experiments.Baseline
import AslBuild.Experiments.MaximumThroughput
import AslBuild.Experiments.ReplicationEffect
import AslBuild.Experiments.StabilityTrace
import AslBuild.Experiments.ThinkTime
import AslBuild.Experiments.WriteEffect
import AslBuild.Jar
import AslBuild.LocalLogTest
import AslBuild.LocalMiddlewareTests
import AslBuild.Orc
import AslBuild.Reports
import AslBuild.Reports.ExperimentFormat
import AslBuild.Test
import AslBuild.Utils

scriptsLib :: FilePath
scriptsLib = scriptsDir </> "lib.sh"

preCommitRule :: String
preCommitRule = "pre-commit"

preCommitRules :: Rules ()
preCommitRules = do
    preCommitRule ~> do
        mapM_
            (need . (: []))
            [ outputJarFile
            , memcachedBin
            , memaslapBin
            , codeHealthRule
            , testRule
            , localLogTestRule
            , localMiddlewareTestsRule
            ]
        liftIO $ do
            removeFiles "" [experimentLocalTmpDir smallLocalBaselineExperiment]
            removeFiles "" [experimentLocalTmpDir smallLocalStabilityTrace]
            removeFiles "" [experimentLocalTmpDir smallLocalMaximumThroughput]
            removeFiles "" [experimentLocalTmpDir smallLocalReplicationEffect]
            removeFiles "" [experimentLocalTmpDir smallLocalWriteEffect]
            removeFiles "" [experimentLocalTmpDir smallLocalThinkTime]
        mapM_
            (need . (: []))
            [ experimentTarget smallLocalBaselineExperiment
            , experimentTarget smallLocalStabilityTrace
            , experimentTarget smallLocalMaximumThroughput
            , experimentTarget smallLocalReplicationEffect
            , experimentTarget smallLocalWriteEffect
            , experimentTarget smallLocalThinkTime
            , analysisRule
            , experimentTablesRule
            , reportsRule
            , formatCabalFileRule
            , formatHindentRule
            , formatClientRule
            ]
        unit $ cmd (Cwd aslDir) gitCmd "add" "." -- Re-add files that were formatted.
        unit $ cmd (Cwd aslDir) "scripts/lines.sh"
    codeHealth
    formatCabalFile
    formatHindent
    formatClient

codeHealthRule :: String
codeHealthRule = "code-health"

codeHealth :: Rules ()
codeHealth = do
    codeHealthRule ~> need [trailingWhitespaceRule, hlintRule, sanityRule]
    trailingWhitespace
    hlint
    sanity

trailingWhitespaceRule :: String
trailingWhitespaceRule = "trailing-whitespace"

trailingWhitespaceScript :: String
trailingWhitespaceScript = scriptsDir </> "trailing_whitespace_test.sh"

trailingWhitespace :: Rules ()
trailingWhitespace =
    trailingWhitespaceRule ~> do
        need [scriptsLib, trailingWhitespaceScript]
        cmd (Cwd aslDir) "scripts/trailing_whitespace_test.sh"

hlintRule :: String
hlintRule = "hlint"

hlintScript :: FilePath
hlintScript = scriptsDir </> "hlint_health.sh"

hlint :: Rules ()
hlint =
    hlintRule ~> do
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
            , "-fno-warn-missing-methods"
            ]
    cmd (Cwd dir) stackCmd "build" "--fast" "--ghc-options" [unwords ghcOpts]

javaFormatterUrl :: FilePath
javaFormatterUrl =
    "https://github.com/google/google-java-format/releases/download/google-java-format-1.0/google-java-format-1.0-all-deps.jar"

javaFormatter :: FilePath
javaFormatter = tmpDir </> "google-java-format-1.0-all-deps.jar"

formatCabalFileRule :: String
formatCabalFileRule = "format-cabal"

formatCabalFile :: Rules ()
formatCabalFile =
    formatCabalFileRule ~> do
        let cabalPath = aslBuildDir </> "orchestra.cabal"
        need [cabalPath]
        cmd "cabal" "format" cabalPath

formatHindentRule :: String
formatHindentRule = "format-hindent"

formatHindent :: Rules ()
formatHindent =
    formatHindentRule ~> do
        files <- getDirectoryFiles "" [aslBuildDir <//> "*.hs"]
        need files
        forP_ files formatHindentFile

formatHindentFile :: FilePath -> Action ()
formatHindentFile file = do
    Exit c <- cmd "hindent" "--indent-size" "4" "--line-length" "100" file
    case c of
        ExitSuccess -> pure ()
        ExitFailure _ ->
            putLoud $
            unwords
                ["hindent failed with exit code", show c, "on file", file, "ignoring this problem."]

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
    javaFormatter %> \_ -> cmd wgetCmd javaFormatterUrl "--output-document" javaFormatter

javaFormat :: FilePath -> Action ()
javaFormat abspath = do
    need [javaFormatter]
    Stdout formattedSrc <- cmd javaCmd "-jar" javaFormatter abspath
    writeFileChanged abspath formattedSrc
