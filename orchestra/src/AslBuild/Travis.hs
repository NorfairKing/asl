module AslBuild.Travis where

import           Development.Shake

--import           AslBuild.Analysis
--import           AslBuild.Baseline
import           AslBuild.BuildMemcached
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.LocalLogTest
import           AslBuild.LocalMiddlewareMultipleClientsTest
import           AslBuild.LocalMiddlewareMultipleServersTest
import           AslBuild.LocalMiddlewareParseTest
import           AslBuild.LocalMiddlewareSimpleTest
import           AslBuild.LocalMiddlewareThoroughTest
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
        ]
    need [localMiddlewareParseTestRule]
    need [localMiddlewareSimpleTestRule]
    need [localMiddlewareMultipleServersTestRule]
    need [localMiddlewareMultipleClientsTestRule]
    need [localMiddlewareThoroughTestRule]

    need [provisionLocalhostRule]
    -- need
    --     [ smallLocalBaselineExperimentRule
    --     , analysisRule
    --     ]
