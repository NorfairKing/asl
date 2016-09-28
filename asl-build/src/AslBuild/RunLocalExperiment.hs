module AslBuild.RunLocalExperiment where

import           System.Process

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.OptParse

localExperimentRules :: AslBuilder ()
localExperimentRules = do
    c <- ask
    lift $ do
        case c of
            BuildAll _ -> want [localExperimentRule]
            BuildRunExperiment LocalExperiment -> want [localExperimentRule]
            _ -> return ()

        localExperiment

localExperimentRule :: String
localExperimentRule = "local-logfile-test"

logFile :: FilePath
logFile = tmpDir </> "local_logfile_test_log.txt"

memaslapConfigFile :: FilePath
memaslapConfigFile = tmpDir </> "local_logfile_test_memaslap_cfg.txt"

csvOut :: FilePath
csvOut = resultsDir </> "local_logfile_test.csv"

msFlags :: MemaslapFlags
msFlags = MemaslapFlags
    { msServers = [RemoteServerUrl localhost defaultMemcachedPort]
    , msThreads = 64
    , msConcurrency = 64
    , msOverwrite = 1
    , msStatFreq = Seconds 2
    , msTime = Seconds 2
    , msConfigFile = memaslapConfigFile
    }

localExperiment :: Rules ()
localExperiment = do
    phony localExperimentRule $ need [csvOut]
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
            runMemaslap = command [FileStdout logFile] memaslapBin (memaslapArgs msFlags)

        -- Make sure to stop memcached
        actionFinally
            runMemaslap
            (terminateProcess ph)

    csvOut %>  \_ -> do
        explog <- readFile' logFile
        case parseLog explog of
            Nothing -> fail "Could not parse logfile."
            Just parsedLog -> writeFile' csvOut $ memaslapLogsCsv [parsedLog]
