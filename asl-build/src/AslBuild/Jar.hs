module AslBuild.Jar where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

jarRules :: AslBuilder ()
jarRules = lift $ do
    let jarFile = asl <.> jar
        jarout = outDir </> jarFile
    want [jarout]

    let jarInGradleBuildDir = codeSrcDir </> build </> libs </> jarFile
    jarInGradleBuildDir %> \_ -> do
        let buildFile = codeSrcDir </> build <.> gradle
            settingsFile = codeSrcDir </> settings <.> gradle
        need [buildFile, settingsFile]
        sourceFiles <- absFilesInDir javaSourceDir ["//*" <.> java]
        need sourceFiles
        let jarTarget = jar
            gradleCmd = gradle
        cmd (Cwd codeSrcDir) gradleCmd jarTarget

    jarout `byCopying` jarInGradleBuildDir
