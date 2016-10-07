module AslBuild.Jar where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Utils

outputJarFile :: FilePath
outputJarFile = outDir </> asl <.> jar

buildFile :: FilePath
buildFile = aslDir </> build <.> xmlExt

jarInBuildDir :: FilePath
jarInBuildDir = aslDir </> dist </> "middleware-tomk" <.> jarExt

cleanJarRule :: String
cleanJarRule = "clean-jar"

jarRule :: String
jarRule = "jar"

jarRules :: Rules ()
jarRules = do
    jarRule ~> need [outputJarFile]

    jarInBuildDir %> \_ -> do
        sourceFiles <- absFilesInDir javaSourceDir ["//*" <.> java]
        need $ buildFile : sourceFiles
        let jarTarget = jar
        cmd (Cwd aslDir) antCmd jarTarget

    outputJarFile `byCopying` jarInBuildDir

    cleanJarRule ~> do
        removeFilesAfter outDir [outputJarFile]
        removeFilesAfter "" ["//dist"]
        removeFilesAfter codeSrcDir ["//build", "//out", "//dist"]
