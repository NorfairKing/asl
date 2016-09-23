module AslBuild.Build where

import           Development.Shake

import           Control.Monad.Reader

import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.OptParse
import           AslBuild.Reports
import           AslBuild.Test

doTheShake :: BuildContext -> IO ()
doTheShake bctx = shakeArgs shakeOptions $
    flip runReaderT bctx $ do
        commitHashRules
        jarRules
        memcachedRules
        reportRules
        testRules
