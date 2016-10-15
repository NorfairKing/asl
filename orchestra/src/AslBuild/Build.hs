module AslBuild.Build where

import           Development.Shake
import           Development.Shake.Config

import           AslBuild.Analysis
import           AslBuild.BuildMemcached
import           AslBuild.Clean
import           AslBuild.CommitHash
import           AslBuild.Create
import           AslBuild.Experiments
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.LocalMiddlewareMultiClientTest
import           AslBuild.LocalMiddlewareMultipleClientsTest
import           AslBuild.LocalMiddlewareMultipleServersTest
import           AslBuild.LocalMiddlewareParseTest
import           AslBuild.LocalMiddlewareReplicationTest
import           AslBuild.LocalMiddlewareSimpleTest
import           AslBuild.LocalMiddlewareThoroughTest
import           AslBuild.Orc
import           AslBuild.PreCommit
import           AslBuild.Provision
import           AslBuild.Reports
import           AslBuild.RunDebug
import           AslBuild.Ssh
import           AslBuild.Test
import           AslBuild.Travis
import           AslBuild.VisualVm
import           AslBuild.Vm

doTheShake :: IO ()
doTheShake = shakeArgs args theShake
  where args = shakeOptions {shakeVerbosity = Loud}

theShake :: Rules ()
theShake  = do
    usingConfigFile "config.cfg"

    commitHashRules
    orcRules
    jarRules
    buildMemcachedRules
    reportRules
    testRules
    localLogTestRules
    localMiddlewareMultipleClientsTestRules
    localMiddlewareMultipleServersTestRules
    localMiddlewareParseTestRules
    localMiddlewareMultiClientTestRules
    localMiddlewareReplicationTestRules
    localMiddlewareSimpleTestRules
    localMiddlewareThoroughTestRules
    experimentRules
    visualVmRules
    runDebugRules
    analysisRules
    sshRules
    createRules
    provisionRules
    vmRules
    cleanRules
    preCommitRules
    travisRules

    allRule ~> need
        [ commithashRule
        , jarRule
        , memcachedRule
        , reportsRule
        , analysisRule
        ]

allRule :: String
allRule = "all"
