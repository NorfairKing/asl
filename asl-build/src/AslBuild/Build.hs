module AslBuild.Build where

import           Development.Shake

import           Control.Monad.Reader

import           AslBuild.Clean
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.OptParse
import           AslBuild.Reports

doTheShake :: BuildContext -> IO ()
doTheShake bctx = shakeArgs shakeOptions $
    flip runReaderT bctx $ do
        commitHashRules
        jarRules
        reportRules
        cleanRules
