module AslBuild.Analysis.BuildR where

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Utils

buildRRules :: Rules ()
buildRRules = do
    rRules
    mapM_ rlib rLibs

rlib :: String -> Rules ()
rlib name =
    rLibTarget name %> \_ -> do
        need [rBin]
        let tmpInstallScript = "/tmp/install-" ++ name <.> "r"
        writeFile' tmpInstallScript $ unlines
            [ "repos <- \"http://cran.rstudio.com\""
            , "libloc <- \"" ++ rlibdir ++ "\""
            , "update.packages(repos=repos, ask=FALSE, lib=libloc)"
            , "install.packages(c(\"" ++ name ++ "\"), repos=repos, lib=libloc)"
            ]
        cmd rBin tmpInstallScript

rLibs :: [String]
rLibs =
    [ "igraph"
    , "caTools"
    , "pkgmaker"
    , "ggplot2"
    , "R.matlab"
    , "RJSONIO"
    ]

rArchive :: FilePath
rArchive = tmpDir </> "R.tar.gz"

rVersion :: String
rVersion = "R-3.2.0"

rLink :: FilePath
rLink = "https://cran.r-project.org/src/base/R-3" </> rVersion <.> tarGzExt

rDir :: FilePath
rDir = tmpDir </> "R"

rBin :: FilePath
rBin = outDir </> "Rscript"

rInstallBin :: FilePath
rInstallBin = outDir </> "R"

rMakeDir :: FilePath
rMakeDir = rDir </> rVersion

rConfigureScriptName :: FilePath
rConfigureScriptName = "configure"

rConfigureScript :: FilePath
rConfigureScript = rMakeDir </> rConfigureScriptName

rMakefile :: FilePath
rMakefile = rMakeDir </> "Makefile"

rBinInMakeDir :: FilePath
rBinInMakeDir = rMakeDir </> "bin" </> "Rscript"

rInstallBinInMakeDir :: FilePath
rInstallBinInMakeDir = rMakeDir </> "bin" </> "R"

rlibdir :: FilePath
rlibdir = rMakeDir </> "library"

rRules :: Rules ()
rRules = do
    rArchive %> \_ ->
        cmd curlCmd
            "--output" rArchive
            rLink

    rConfigureScript %> \_ -> do
        need [rArchive]
        cmd tarCmd
            "--extract"
            "--verbose"
            "--file" rArchive
            "--directory" rDir

    rMakefile %> \_ -> do
        need [rConfigureScript]
        cmd (Cwd rMakeDir) ("." </> rConfigureScriptName)

    [rBinInMakeDir, rInstallBinInMakeDir] &%> \_ -> do
        need [rMakefile]
        cmd (Cwd rMakeDir) "make" "--jobs"

    rBin `byCopying` rBinInMakeDir
    rInstallBin `byCopying` rInstallBinInMakeDir



rScript :: CmdArguments args => args
rScript = cmd rBin (AddEnv "R_LIBS" rlibdir)

rLibTarget :: String -> FilePath
rLibTarget name = rlibdir </> name </> "R" </> name

needRLibs :: [String] -> Action ()
needRLibs names = need $ map rLibTarget names
