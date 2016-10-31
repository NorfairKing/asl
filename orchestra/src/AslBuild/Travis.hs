module AslBuild.Travis where

import           Development.Shake

import           AslBuild.Analysis
import           AslBuild.BuildMemcached
import           AslBuild.Experiments.Baseline
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.LocalMiddlewareMultiClientTest
import           AslBuild.LocalMiddlewareMultipleClientsTest
import           AslBuild.LocalMiddlewareMultipleServersTest
import           AslBuild.LocalMiddlewareParseTest
import           AslBuild.LocalMiddlewareReplicationTest
import           AslBuild.LocalMiddlewareSimpleTest
import           AslBuild.Orc
import           AslBuild.Provision
import           AslBuild.Reports
import           AslBuild.Test

travisRule :: String
travisRule = "travis"

travisRules :: Rules ()
travisRules = travisRule ~> do
    need
        [ outputJarFile
        , orcBin
        , memaslapBin
        , memcachedBin
        ]

    need
        [ testRule
        , localLogTestRule
        ]

    need [localMiddlewareParseTestRule]
    need [localMiddlewareMultiClientTestRule]
    need [localMiddlewareReplicationTestRule]
    need [localMiddlewareSimpleTestRule]
    need [localMiddlewareMultipleServersTestRule]
    need [localMiddlewareMultipleClientsTestRule]

    need [provisionLocalhostRule]
    need [smallLocalBaselineExperimentRule]
    need [smallLocalStabilityTraceRule]
    need [analysisRule]
    need [reportsRule]
