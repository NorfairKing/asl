module AslBuild.Build where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad.Reader

import           AslBuild.Clean
import           AslBuild.CommitHash
import           AslBuild.Jar
import           AslBuild.OptParse
import           AslBuild.Reports

doTheShake :: BuildContext -> IO ()
doTheShake bctx = shakeArgs shakeOptions $
    flip runReaderT bctx $ do
        commitHashRules
        jarRules
        reportRules
        cleanRules

        lift $ do
            want ["test"]

            let libdir = "asl/lib"
            let junitjar = libdir </> "junit.jar"
            let hamcrestjar = libdir </> "hamcrest-core.jar"
            let truthjar = libdir </> "truth.jar"
            let guavajar = libdir </> "guava.jar"
            phony "test" $
                need [junitjar, hamcrestjar, truthjar, guavajar]

            javalib
                junitjar
                "http://central.maven.org/maven2/junit/junit/4.12/junit-4.12.jar"

            javalib
                hamcrestjar
                "http://central.maven.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar"

            javalib
                truthjar
                "http://central.maven.org/maven2/com/google/truth/truth/0.30/truth-0.30.jar"

            javalib
                guavajar
                "http://central.maven.org/maven2/com/google/guava/guava/19.0/guava-19.0.jar"

javalib :: FilePath -> String -> Rules ()
javalib out url = out %> cmd "curl" "-o" out url
