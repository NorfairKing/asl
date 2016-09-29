module AslBuild.Jar where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Utils

outputJarFile :: FilePath
outputJarFile = outDir </> asl <.> jar

cleanJarRule :: String
cleanJarRule = "cleanjar"

jarRule :: String
jarRule = "jar"

jarRules :: Rules ()
jarRules = do
    jarRule ~> need [outputJarFile]

    let jarFile = asl <.> jar
    let jarInBuildDir = codeSrcDir </> dist </> jarFile

    jarInBuildDir %> \_ -> do
        let buildFile = codeSrcDir </> build <.> xmlExt
            propertiesFile = codeSrcDir </> build <.> propertiesExt
        need [buildFile, propertiesFile]
        sourceFiles <- absFilesInDir javaSourceDir ["//*" <.> java]
        need sourceFiles
        let jarTarget = jar
            antCmd = ant
        cmd (Cwd codeSrcDir) antCmd jarTarget

    outputJarFile `byCopying` jarInBuildDir

    cleanJarRule ~> do
        removeFilesAfter outDir [outputJarFile]
        removeFilesAfter codeSrcDir ["//build", "//out", "//dist"]
