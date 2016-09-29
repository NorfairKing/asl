module AslBuild.Build where

import           Development.Shake

import           Control.Monad.Reader

import           AslBuild.Analysis
import           AslBuild.CommitHash
import           AslBuild.Experiments
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.OptParse
import           AslBuild.PreCommit
import           AslBuild.Reports
import           AslBuild.Test
import           AslBuild.Travis

doTheShake :: BuildContext -> IO ()
doTheShake bctx
    = shakeArgs shakeOptions { shakeVerbosity = Loud }
    $ theShake bctx

theShake :: BuildContext -> Rules ()
theShake bctx = flip runReaderT bctx $ do
    commitHashRules
    jarRules
    memcachedRules
    reportRules
    testRules
    experimentRules
    analysisRules
    preCommitRules
    travisRules
