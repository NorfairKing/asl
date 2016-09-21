module AslBuild.Clean where

import           Development.Shake

import           AslBuild.Constants
import           AslBuild.OptParse

cleanRules :: AslBuilder ()
cleanRules = do
    c <- getCommand
    lift $ do
        case c of
            CommandClean -> want [cleanTarget]
            _ -> return ()
        phony cleanTarget $ do

            removeFilesAfter outDir ["//"]
            removeFilesAfter codeSrcDir ["//build", "//out"]
            removeFilesAfter reportsDir ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

