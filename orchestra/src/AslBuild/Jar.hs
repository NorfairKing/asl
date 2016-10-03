module AslBuild.Jar where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Utils

outputJarFile :: FilePath
outputJarFile = outDir </> asl <.> jar

cleanJarRule :: String
cleanJarRule = "clean-jar"

jarRule :: String
jarRule = "jar"

jarRules :: Rules ()
jarRules = do
    jarRule ~> need [outputJarFile]

    let jarInBuildDir = dist </> "middleware-tomk" <.> jarExt

    jarInBuildDir %> \_ -> do
        let buildFile = build <.> xmlExt
        sourceFiles <- absFilesInDir javaSourceDir ["//*" <.> java]
        need $ buildFile : sourceFiles
        let jarTarget = jar
            antCmd = ant
        cmd antCmd jarTarget

    outputJarFile `byCopying` jarInBuildDir

    cleanJarRule ~> do
        removeFilesAfter outDir [outputJarFile]
        removeFilesAfter "" ["//dist"]
        removeFilesAfter codeSrcDir ["//build", "//out", "//dist"]
