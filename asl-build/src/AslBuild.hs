module AslBuild
    ( aslBuild
    ) where

import           Development.Shake
import           Development.Shake.FilePath


aslBuild :: IO ()
aslBuild = do
    putStrLn "ASL Build system"
    shakeArgs shakeOptions $ do
        let outdir = "out"
            jarfile = "asl.jar"
            jarout = outdir </> jarfile
        want [jarout]
        let jarInGradleBuildDir = "asl/build/libs/asl-0.1.jar"
        jarInGradleBuildDir %> \out -> do
            let buildFile = "asl/build.gradle"
            need [buildFile]
            cmd "gradle" "--build-file" buildFile "jar"
        jarout %> \out -> do
            need [jarInGradleBuildDir]
            cmd "mv" jarInGradleBuildDir out
