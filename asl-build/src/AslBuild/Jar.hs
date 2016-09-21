module AslBuild.Jar where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

outputJarFile :: FilePath
outputJarFile = outDir <.> jar

jarRules :: AslBuilder ()
jarRules = do
    c <- getCommand
    lift $ do
        case c of
            CommandBuild -> want [outputJarFile]
            _ -> return ()

        let jarFile = asl <.> jar
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

        outputJarFile `byCopying` jarInGradleBuildDir
