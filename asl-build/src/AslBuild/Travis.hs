module AslBuild.Travis where

import           Development.Shake

import           AslBuild.Analysis
import           AslBuild.Baseline
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.Memcached
import           AslBuild.Test

travisRule :: String
travisRule = "travis"

travisRules :: Rules ()
travisRules = travisRule ~> need
    [ commithashFile
    , outputJarFile
    , memaslapBin
    , memcachedBin
    , testRule

    -- Experiments
    , localLogTestRule
    , localBaselineExperimentRule

    , analysisRule
    ]
