module AslBuild.LocalLogTest where

import           System.Process

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Types

localLogTestRule :: String
localLogTestRule = "local-logfile-test"

logFile :: FilePath
logFile = tmpDir </> "local-logfile-test-log.txt"

memaslapConfigFile :: FilePath
memaslapConfigFile = tmpDir </> "local-logfile-test-memaslap-cfg.txt"

csvOut :: FilePath
csvOut = resultsDir </> "local-logfile-test.csv"

currentMemaslapFlags :: MemaslapFlags
currentMemaslapFlags = MemaslapFlags
    { msServers = [RemoteServerUrl "localhost" defaultMemcachedPort]
    , msThreads = 64
    , msConcurrency = 64
    , msOverwrite = 1
    , msStatFreq = Seconds 2
    , msTime = Seconds 2
    , msConfigFile = memaslapConfigFile
    }

localLogTestRules :: Rules ()
localLogTestRules = do
    phony localLogTestRule $ need [csvOut]
    logFile %> \_ -> do
        need [memcachedBin, memaslapBin]

        -- Choose a memaslap config and write it to the config file.
        let memaslapConfig = MemaslapConfig
                { keysizeDistributions = [Distribution 128 128 1]
                , valueDistributions = [Distribution 2048 2048 1]
                , setProportion = 0.1
                , getProportion = 0.9
                }
        writeMemaslapConfig memaslapConfigFile memaslapConfig

        -- Start memcached locally
        ph <- cmd memcachedBin

        -- Run memaslap locally
        let runMemaslap :: Action ()
            runMemaslap = command [FileStdout logFile] memaslapBin (memaslapArgs currentMemaslapFlags)

        -- Make sure to stop memcached
        actionFinally
            runMemaslap
            (terminateProcess ph)

    csvOut %>  \_ -> do
        explog <- readFile' logFile
        case parseLog explog of
            Nothing -> fail "Could not parse logfile."
            Just parsedLog -> writeFile' csvOut $ memaslapLogsCsv [parsedLog]
