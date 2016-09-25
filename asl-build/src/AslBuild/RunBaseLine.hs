{-# LANGUAGE RecordWildCards #-}
module AslBuild.RunBaseLine where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Concurrent
import           Data.Hashable
import           System.Directory

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
logFile = "tmp/baseline_memaslaplog.txt"

memaslapConfigFile :: FilePath
memaslapConfigFile = "tmp/distribution.txt"

csvOut :: FilePath
csvOut = "out/baseline_experiment.csv"

wait :: Int -> Action ()
wait i = liftIO $ threadDelay $ i * 1000 * 1000

data Script
    = Script
    { scriptName    :: FilePath
    , scriptContent :: [String]
    }

namedScript :: FilePath -> [String] -> Script
namedScript name contents = Script name $ "#!/bin/bash" : contents

script :: [String] -> Script
script contents = namedScript (show $ hash contents) contents

tmpMemaslap :: FilePath
tmpMemaslap = "/tmp/memaslap"

tmpMemaslapCfg :: FilePath
tmpMemaslapCfg = "/tmp/memaslapcfg"

tmpMemcached :: FilePath
tmpMemcached = "/tmp/memcached"

startMemcachedScript :: Script
startMemcachedScript = Script
    "start-memcached-detached"
    ["screen -d -m " ++ tmpMemcached]

baselineExperiment :: Rules ()
baselineExperiment = do
    phony baselineExperimentRule $ need [csvOut]

    logFile %> \_ -> do
        let localhost = RemoteLogin "syd" "localhost"
        let client = localhost
        let server = localhost

        -- Copy memaslap and its config to the client
        rsyncTo client memaslapBin tmpMemaslap
        rsyncTo client memaslapConfigFile tmpMemaslapCfg

        -- Copy memcached and its config to the server
        rsyncTo server memcachedBin tmpMemcached

        -- Start memcached on the server
        scriptAt server startMemcachedScript

        -- Wait for the server to get started
        wait 1

        -- Time to run the experiment for
        let timeInSeconds = 2

        let flags = MemaslapFlags
                { msServers = [RemoteServerUrl "localhost" 11211]
                , msThreads = 64
                , msConcurrency = 64
                , msOverwrite = 1
                , msStatFreq = Seconds timeInSeconds
                , msTime = Seconds timeInSeconds
                , msConfigFile = tmpMemaslapCfg
                }

        let remoteLog = "/tmp/memaslaplog"
        let memaslapScript = script
                [ unwords
                $ tmpMemaslap : memaslapArgs flags
                    ++ [">", remoteLog, "2>&1", "&"]
                ]

        scriptAt client memaslapScript

        -- Wait long enough to be sure that memaslap is fully done.
        wait (timeInSeconds + 5)

        -- Make sure no memcached servers are running anymore
        overSsh server $ unwords ["killall", memcachedBinName]

        rsyncFrom client remoteLog logFile

    csvOut %>  \_ -> do
        explog <- readFile' logFile
        case parseLog explog of
            Nothing -> fail "Could not parse logfile."
            Just parsedLog -> writeFile' csvOut $ parsedLogsCsv [parsedLog]


scriptAt :: RemoteLogin -> Script -> Action ()
scriptAt rl Script{..} = do
    let path = "/tmp" </> scriptName <.> "bash"
    let fullScript = unlines scriptContent
    writeFile' path fullScript
    -- Make sure it's executable before we copy the script
    -- rsync will retain the permissions.
    liftIO $ do
        p <- getPermissions path
        setPermissions path (p {executable = True})
    -- Copy over the script
    rsyncTo rl path path
    liftIO $ putStrLn $ "Running on " ++ remoteLoginStr rl ++ ":\n" ++ fullScript
    -- Run the script
    overSsh rl path

overSsh :: RemoteLogin -> String -> Action ()
overSsh rl commandOverSsh =
    command [] "ssh" [remoteLoginStr rl, commandOverSsh]

rsyncTo :: RemoteLogin -> FilePath -> FilePath -> Action ()
rsyncTo rl localThing remoteThing = do
    need [localThing]
    command [] "rsync"
        [ localThing
        , remoteLoginStr rl ++ ":" ++ remoteThing
        ]

rsyncFrom :: RemoteLogin -> FilePath -> FilePath -> Action ()
rsyncFrom rl remoteThing localThing =
    command [] "rsync"
        [ remoteLoginStr rl ++ ":" ++ remoteThing
        , localThing
        ]

remoteLoginStr :: RemoteLogin -> String
remoteLoginStr RemoteLogin{..} = remoteUser ++ "@" ++ remoteHost

data RemoteLogin
    = RemoteLogin
    { remoteUser :: String
    , remoteHost :: String
    }
