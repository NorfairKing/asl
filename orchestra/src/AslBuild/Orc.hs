module AslBuild.Orc where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Utils

buildBinInStack :: FilePath
buildBinInStack = aslDir </> ".stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin/orc"

aslBuildDir :: FilePath
aslBuildDir = aslDir </> "orchestra"

orcBin :: FilePath
orcBin = outDir </> "orc"

orcRules :: Rules ()
orcRules = do
    orcBin `byCopying` buildBinInStack
    buildBinInStack %> \_ -> do
        files <- getDirectoryFiles "" [aslBuildDir <//> "*.hs"]
        need files
        cmd (Cwd aslBuildDir) stackCmd "build"

