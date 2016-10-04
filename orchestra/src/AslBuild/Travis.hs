module AslBuild.Travis where

import           Development.Shake

import           AslBuild.Analysis
import           AslBuild.Baseline
import           AslBuild.BuildMemcached
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.LocalMiddlewareParseTest
-- import           AslBuild.LocalMiddlewareTest
import           AslBuild.Provision
import           AslBuild.Test

travisRule :: String
travisRule = "travis"

travisRules :: Rules ()
travisRules = travisRule ~> do
    need
        [ commithashFile
        , outputJarFile
        , memaslapBin
        , memcachedBin
        , testRule
        , localLogTestRule
        , localMiddlewareParseTestRule
        -- , localMiddlewareTestRule Doesn't work yet...
        ]

    need [provisionLocalhostRule]
    need
        [ localBaselineExperimentRule
        , analysisRule
        ]
