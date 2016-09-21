module AslBuild.Build where

import           Development.Shake

import           Control.Monad.Reader

import           AslBuild.Clean
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.OptParse
import           AslBuild.Reports

doTheShake :: Arguments -> IO ()
doTheShake args = shakeArgs shakeOptions $
    flip runReaderT args $ do
        commitHashRules
        jarRules
        reportRules
        cleanRules
