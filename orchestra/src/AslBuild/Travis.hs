module AslBuild.Travis where

import           Development.Shake

import           AslBuild.Analysis
import           AslBuild.BuildMemcached
import           AslBuild.Experiments.Baseline
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.LocalMiddlewareTest.MultiClientTest
import           AslBuild.LocalMiddlewareTest.MultipleClientsTest
import           AslBuild.LocalMiddlewareTest.MultipleServersTest
import           AslBuild.LocalMiddlewareTest.ParseTest
import           AslBuild.LocalMiddlewareTest.ReplicationTest
import           AslBuild.LocalMiddlewareTest.SimpleTest
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
    need [smallLocalReplicationEffectRule]

    need [analysisRule]
    need [reportsRule]
