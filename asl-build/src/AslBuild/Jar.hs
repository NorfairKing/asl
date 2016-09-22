module AslBuild.Jar where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

outputJarFile :: FilePath
outputJarFile = outDir </> asl <.> jar

jarRules :: AslBuilder ()
jarRules = do
    c <- ask
    lift $ do
        case c of
            BuildAll _ -> want [outputJarFile]
            _ -> return ()

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
