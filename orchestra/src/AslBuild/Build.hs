module AslBuild.Build where

import Development.Shake

import AslBuild.Analysis
import AslBuild.BuildMemcached
import AslBuild.Clean
import AslBuild.CommitHash
import AslBuild.Create
import AslBuild.Experiments
import AslBuild.IRTL
import AslBuild.Jar
import AslBuild.LocalLogTest
import AslBuild.LocalMiddlewareTests
import AslBuild.Models
import AslBuild.Orc
import AslBuild.PreCommit
import AslBuild.Provision
import AslBuild.Reports
import AslBuild.RunDebug
import AslBuild.Ssh
import AslBuild.Test
import AslBuild.VisualVm
import AslBuild.Vm

doTheShake :: IO ()
doTheShake = shakeArgs args theShake
  where
    args =
        shakeOptions
        { shakeVerbosity = Loud
        , shakeThreads = 0 -- Use as many threads as processors
        }

theShake :: Rules ()
theShake = do
    commitHashRules
    orcRules
    jarRules
    buildMemcachedRules
    reportRules
    testRules
    localLogTestRules
    localMiddlewareTestRules
    experimentRules
    visualVmRules
    runDebugRules
    analysisRules
    modelsRules
    irtlRules
    sshRules
    createRules
    provisionRules
    vmRules
    cleanRules
    preCommitRules
