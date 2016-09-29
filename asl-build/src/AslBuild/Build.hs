module AslBuild.Build where

import           Development.Shake

import           AslBuild.Analysis
import           AslBuild.Clean
import           AslBuild.CommitHash
import           AslBuild.Experiments
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.PreCommit
import           AslBuild.Reports
import           AslBuild.Test
import           AslBuild.Travis

doTheShake :: IO ()
doTheShake = shakeArgs args theShake
  where args = shakeOptions {shakeVerbosity = Loud}

theShake :: Rules ()
theShake  = do
    commitHashRules
    jarRules
    memcachedRules
    reportRules
    testRules
    experimentRules
    analysisRules
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
