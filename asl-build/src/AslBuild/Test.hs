module AslBuild.Test where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad.Reader

import           AslBuild.Constants
import           AslBuild.OptParse

testRule :: String
testRule = "test"

cleanTestRule :: String
cleanTestRule = "cleantest"

testRules :: AslBuilder ()
testRules = do
    c <- ask

    lift $ do
        case c of
            BuildTest -> want [testRule]
            BuildClean -> want [cleanTestRule]
            _ -> return ()

        testRule ~> do
            need $ map fst javadeps
            cmd (Cwd codeSrcDir) ant "test"

        cleanTestRule ~> removeFilesAfter javalibdir ["//"]

        mapM_ (uncurry javalib) javadeps



javalibdir :: FilePath
javalibdir = codeSrcDir </> lib

javalib :: FilePath -> String -> Rules ()
javalib outputfile url = outputfile %> \_ -> cmd curlCmd "--output" outputfile url

javadeps :: [(FilePath, String)]
javadeps = map (\(name, url) -> (javalibdir </> name <.> jar, url))
    [ ("junit", "http://central.maven.org/maven2/junit/junit/4.12/junit-4.12.jar")
    , ("junit-quickcheck-core", "http://central.maven.org/maven2/com/pholser/junit-quickcheck-core/0.6.1/junit-quickcheck-core-0.6.1.jar")
    , ("junit-quickcheck-generators", "http://central.maven.org/maven2/com/pholser/junit-quickcheck-generators/0.6.1/junit-quickcheck-generators-0.6.1.jar")
    , ("hamcrest-core", "http://central.maven.org/maven2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar")
    , ("truth", "http://central.maven.org/maven2/com/google/truth/truth/0.30/truth-0.30.jar")
    , ("guava", "http://central.maven.org/maven2/com/google/guava/guava/19.0/guava-19.0.jar")
    ]
