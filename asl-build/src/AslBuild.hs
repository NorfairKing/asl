module AslBuild
    ( aslBuild
    ) where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.OptParse

aslBuild :: IO ()
aslBuild = do
    (Command, flags) <- getArguments

    putStrLn "ASL Build system"
    shakeArgs shakeOptions $ do
        jarRules
        reportRules

jarRules :: Rules ()
jarRules = do
    let outdir = "out"
        jarfile = "asl.jar"
        jarout = outdir </> jarfile
    want [jarout]

    let jarInGradleBuildDir = "asl/build/libs/asl.jar"
    jarInGradleBuildDir %> \out -> do
        let buildFile = "asl/build.gradle"
        need [buildFile]
        cmd "gradle" "--build-file" buildFile "jar"

    jarout %> \out -> do
        need [jarInGradleBuildDir]
        cmd "mv" jarInGradleBuildDir out

reportRules :: Rules ()
reportRules = do
    return ()
