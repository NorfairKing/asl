module AslBuild.Build where

import           Development.Shake

import           Control.Monad.Reader

import           AslBuild.Clean
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.OptParse
import           AslBuild.Reports

doTheShake :: Flags -> IO ()
doTheShake flags = shakeArgs shakeOptions $
    flip runReaderT flags $ do
        commitHashRules
        jarRules
        reportRules
        cleanRules
