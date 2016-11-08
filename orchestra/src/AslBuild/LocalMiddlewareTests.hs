module AslBuild.LocalMiddlewareTests where

import           Development.Shake

import           AslBuild.LocalMiddlewareTest.MultiClientTest
import           AslBuild.LocalMiddlewareTest.MultipleClientsTest
import           AslBuild.LocalMiddlewareTest.MultipleServersTest
import           AslBuild.LocalMiddlewareTest.ParseTest
import           AslBuild.LocalMiddlewareTest.ReplicationFailureTest
import           AslBuild.LocalMiddlewareTest.ReplicationTest
import           AslBuild.LocalMiddlewareTest.SimpleTest

localMiddlewareTestsRule :: String
localMiddlewareTestsRule = "local-middleware-tests"

localMiddlewareTestRules :: Rules ()
localMiddlewareTestRules = do
    localMiddlewareMultipleClientsTestRules
    localMiddlewareMultipleServersTestRules
    localMiddlewareParseTestRules
    localMiddlewareMultiClientTestRules
    localMiddlewareReplicationTestRules
    localMiddlewareReplicationFailureTestRules
    localMiddlewareSimpleTestRules

    localMiddlewareTestsRule ~> mapM_ (need . (:[]))
        [ localMiddlewareParseTestRule
        , localMiddlewareMultiClientTestRule
        , localMiddlewareReplicationTestRule
        , localMiddlewareReplicationFailureTestRule
        , localMiddlewareSimpleTestRule
        , localMiddlewareMultipleServersTestRule
        , localMiddlewareMultipleClientsTestRule
        ]
