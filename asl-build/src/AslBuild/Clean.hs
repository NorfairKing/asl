module AslBuild.Clean where

import           Development.Shake

import           AslBuild.Constants
import           AslBuild.OptParse

cleanRules :: AslBuilder ()
cleanRules = lift $ phony "clean" $ do
    removeFilesAfter outDir ["//"]
    removeFilesAfter codeSrcDir ["//build", "//out"]
    removeFilesAfter reportsDir ["//*.pdf", "//*.aux", "//*.log", "//*.fls", "//*.fdb_latexmk"]

