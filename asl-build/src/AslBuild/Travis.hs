module AslBuild.Travis where

import           Development.Shake

import           Control.Monad.Reader

import           AslBuild.Analysis
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.OptParse
import           AslBuild.PreCommit
import           AslBuild.RunLocalExperiment
import           AslBuild.Test

travisRule :: String
travisRule = "travis"

travisRules :: AslBuilder ()
travisRules = do
    c <- ask
    lift $ do
        case c of
            BuildTravis -> want [travisRule]
            _ -> return ()
        travisRule ~> need
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
            , preCommitRule
            ]
