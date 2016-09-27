module AslBuild.Analysis.BuildR where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad.Reader

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

rBuildRules :: AslBuilder ()
rBuildRules = do
    c <- ask
    lift $ do
        case c of
            BuildAll _ -> want [rScriptBin]
            BuildClean -> want [rCleanRule]
            _ -> return ()

        rBuild


rTarballName :: FilePath
rTarballName = "R-3.3.1"

rTarballFileName :: FilePath
rTarballFileName = rTarballName <.> tarGzExt

rTarball :: FilePath
rTarball = tmpDir </> rTarballFileName

rTarballLink :: String
rTarballLink = "https://cran.r-project.org/src/base/R-3" </> rTarballFileName

rDir :: FilePath
rDir = tmp </> rTarballName

rConfigureScript :: FilePath
rConfigureScript = rDir </> configure

rMakefile :: FilePath
rMakefile = rDir </> makefile

rScriptName :: String
rScriptName = "Rscript"

rScriptPath :: FilePath
rScriptPath = bin </> rScriptName

rScriptBinInCacheDir :: FilePath
rScriptBinInCacheDir = rDir </> rScriptPath

rScriptBin :: FilePath
rScriptBin = outDir </> rScriptName

rCleanRule :: String
rCleanRule = "cleanr"

rBuild :: Rules ()
rBuild = do
    rTarball %> \_ ->
        cmd curlCmd "--output" rTarball rTarballLink

    rConfigureScript %> \_ -> do
        need [rTarball]
        cmd tarCmd "--extract" "--verbose" "--gzip" "--directory" tmpDir "--file" rTarball

    rMakefile %> \_ -> do
        need [rConfigureScript]
        cmd (Cwd rDir) ("." </> configure)

    rScriptBinInCacheDir %> \_ -> do
        need [rMakefile]
        cmd (Cwd rDir) makeCmd "--jobs"

    rScriptBin `byCopying` rScriptBinInCacheDir

    rCleanRule ~> removeFilesAfter rDir ["//"]
