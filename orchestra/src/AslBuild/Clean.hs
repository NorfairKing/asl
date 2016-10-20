module AslBuild.Clean where

import           Development.Shake

import           AslBuild.Analysis
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.Reports
import           AslBuild.Test

cleanRule :: String
cleanRule = "clean"

cleanRules :: Rules ()
cleanRules = cleanRule ~> need
    [ cleanCommithashRule
    , cleanAnalysisRule
    , cleanJarRule
    , cleanReportsRule
    , cleanTestRule
    ]
