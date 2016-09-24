{-# LANGUAGE RecordWildCards #-}
module AslBuild.RunBaseLine where

import           Development.Shake

import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.OptParse

baselineExperimentRules :: AslBuilder ()
baselineExperimentRules = do
    c <- ask
    lift $ do
        case c of
            BuildRunExperiment BaselineExperiment -> want [baselineExperimentRule]
            _ -> return ()

        baselineExperiment

baselineExperimentRule :: String
baselineExperimentRule = "baseline-experiment"

logFile :: FilePath
logFile = "tmp/memaslaplog2.txt"

memaslapConfigFile :: FilePath
memaslapConfigFile = "tmp/distribution.txt"

csvOut :: FilePath
csvOut = "out/baseline_experiment.csv"

baselineExperiment :: Rules ()
baselineExperiment = do
    phony baselineExperimentRule $ need [csvOut]

    logFile %> \_ -> do
        let localhost = RemoteLogin "syd" "localhost"
        let client = localhost
        let server = localhost

        let tmpMemaslap = "/tmp/memaslap"
        let tmpMemaslapCfg = "/tmp/memaslapcfg"
        let tmpMemcached = "/tmp/memcached"

        rsyncTo client memaslapBin tmpMemaslap
        rsyncTo client memaslapConfigFile tmpMemaslapCfg
        rsyncTo server memcachedBin tmpMemcached

        -- Start memcached on the server
        overSsh server ("screen -d -m " ++ tmpMemcached)

        let flags = MemaslapFlags
                { msServers = [RemoteServerUrl "localhost" 11211]
                , msThreads = 64
                , msConcurrency = 64
                , msOverwrite = 1
                , msStatFreq = Seconds 2
                , msTime = Seconds 2
                , msConfigFile = tmpMemaslapCfg
                }

        let remoteLog = "/tmp/memaslaplog"

        overSsh client $
            unwords $
                tmpMemaslap : memaslapArgs flags
                ++ [">", remoteLog]

        -- Make sure no memcached servers are running anymore
        overSsh server $ unwords ["killall", memcachedBinName]

        rsyncFrom client remoteLog logFile

    csvOut %>  \_ -> do
        explog <- readFile' logFile
        case parseLog explog of
            Nothing -> fail "Could not parse logfile."
            Just parsedLog -> writeFile' csvOut $ parsedLogsCsv [parsedLog]



overSsh :: RemoteLogin -> String -> Action ()
overSsh rl commandOverSsh =
    command [] "ssh" [remoteLoginStr rl, commandOverSsh]

rsyncTo :: RemoteLogin -> FilePath -> FilePath -> Action ()
rsyncTo rl localThing remoteThing = do
    need [localThing]
    command [] "rsync" [localThing, remoteLoginStr rl ++ ":" ++ remoteThing]

rsyncFrom :: RemoteLogin -> FilePath -> FilePath -> Action ()
rsyncFrom rl remoteThing localThing =
    command [] "rsync" [remoteLoginStr rl ++ ":" ++ remoteThing, localThing]

remoteLoginStr :: RemoteLogin -> String
remoteLoginStr RemoteLogin{..} = remoteUser ++ "@" ++ remoteHost

data RemoteLogin
    = RemoteLogin
    { remoteUser :: String
    , remoteHost :: String
    }
