module AslBuild.Build where

import           Development.Shake

import           AslBuild.Analysis
import           AslBuild.BuildMemcached
import           AslBuild.Clean
import           AslBuild.CommitHash
import           AslBuild.Create
import           AslBuild.Experiments
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.LocalMiddlewareTest.MultiClientTest
import           AslBuild.LocalMiddlewareTest.MultipleClientsTest
import           AslBuild.LocalMiddlewareTest.MultipleServersTest
import           AslBuild.LocalMiddlewareTest.ParseTest
import           AslBuild.LocalMiddlewareTest.ReplicationFailureTest
import           AslBuild.LocalMiddlewareTest.ReplicationTest
import           AslBuild.LocalMiddlewareTest.SimpleTest
import           AslBuild.LocalMiddlewareTest.ThoroughTest
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
  where
    args = shakeOptions
        { shakeVerbosity = Loud
        , shakeThreads = 0 -- Use as many threads as processors
        }

theShake :: Rules ()
theShake  = do
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
    localMiddlewareReplicationFailureTestRules
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
