module AslBuild.Travis where

import           Development.Shake

import           AslBuild.Analysis
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.RunLocalExperiment
import           AslBuild.Test

travisRule :: String
travisRule = "travis"

travisRules :: Rules ()
travisRules = travisRule ~> need
    [ commithashFile
    , outputJarFile
    , memaslapBin
    , memcachedBin
    -- Don't build reports on travis
    , testRule
    -- Experiments
    , localExperimentRule
    -- TODO baseline experiment
    , analysisRule
    ]
