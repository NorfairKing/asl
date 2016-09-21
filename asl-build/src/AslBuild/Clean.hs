module AslBuild.Clean where

import           Development.Shake

import           AslBuild.Constants
import           AslBuild.OptParse

cleanRules :: AslBuilder ()
cleanRules = do
    c <- ask
    lift $ do
        case c of
            BuildClean -> want [cleanTarget]
            _ -> return ()
        phony cleanTarget $ do

            removeFilesAfter outDir ["//"]
            removeFilesAfter codeSrcDir ["//build", "//out"]
            removeFilesAfter reportsDir ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

