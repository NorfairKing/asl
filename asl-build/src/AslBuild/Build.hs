module AslBuild.Build where

import           Development.Shake
import           Development.Shake.Config

import           AslBuild.Analysis
import           AslBuild.Clean
import           AslBuild.CommitHash
import           AslBuild.Create
import           AslBuild.Experiments
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.PreCommit
import           AslBuild.Reports
import           AslBuild.Ssh
import           AslBuild.Test
import           AslBuild.Travis
import           AslBuild.Vm

doTheShake :: IO ()
doTheShake = shakeArgs args theShake
  where args = shakeOptions {shakeVerbosity = Loud}

theShake :: Rules ()
theShake  = do
    usingConfigFile "config.cfg"

    commitHashRules
    jarRules
    memcachedRules
    reportRules
    testRules
    experimentRules
    analysisRules
    sshRules
    createRules
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
