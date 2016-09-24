module AslBuild.RunLocalExperiment where

import           System.Process

import           Development.Shake

import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.OptParse

localExperimentRules :: AslBuilder ()
localExperimentRules = do
    c <- ask
    lift $ do
        case c of
            BuildRunExperiment LocalExperiment -> want [localExperimentRule]
            _ -> return ()

        localExperiment

localExperimentRule :: String
localExperimentRule = "local-experiment"

logFile :: FilePath
logFile = "tmp/memaslaplog.txt"

memaslapConfigFile :: FilePath
memaslapConfigFile = "tmp/distribution.txt"

csvOut :: FilePath
csvOut = "out/local_experiment.csv"

msFlags :: MemaslapFlags
msFlags = MemaslapFlags
    { msServers = [RemoteServerUrl "localhost" 11211]
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
        need [memcachedBin, memaslapConfigFile, memaslapBin]

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
            Just parsedLog -> writeFile' csvOut $ parsedLogsCsv [parsedLog]
