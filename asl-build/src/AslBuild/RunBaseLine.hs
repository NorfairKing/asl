{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.RunBaseLine where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Concurrent
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as LB
import           Data.Csv
import           Data.Hashable
import           GHC.Generics
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
csvOut = "results/baseline_experiment_localhost.csv"

wait :: Int -> Action ()
wait i = do
    putLoud $ unwords ["Waiting for", show i, "seconds."]
    liftIO $ threadDelay $ i * 1000 * 1000

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

remoteMemaslapCfg :: FilePath
remoteMemaslapCfg = "/tmp/memaslapcfg"

tmpMemcached :: FilePath
tmpMemcached = "/tmp/memcached"

startMemcachedScript :: Script
startMemcachedScript = Script
    "start-memcached-detached"
    ["screen -d -m " ++ tmpMemcached]

data ExperimentSetup
    = ExperimentSetup
    { memaslapFlags :: MemaslapFlags
    , localLog      :: FilePath
    , remoteLog     :: FilePath
    , resultsFile   :: FilePath
    } deriving (Show, Eq, Generic)

data ExperimentResults
    = ExperimentResults
    { erMemaslapFlags :: MemaslapFlags
    , erMemaslapLog   :: MemaslapLog
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResults
instance FromJSON ExperimentResults

data BaseLineSetup
    = BaseLineSetup
    { repetitions :: Int
    , runtime     :: Int
    }

baseLineSetup :: BaseLineSetup
baseLineSetup = BaseLineSetup
    { repetitions = 5
    , runtime = 30
    }

localhostname :: String
localhostname = "localhost"

baselineExperiments :: [ExperimentSetup]
baselineExperiments = do
    let BaseLineSetup{..} = baseLineSetup

    let servers = [RemoteServerUrl "localhost" 11211]
    let threads = length servers
    rep <- [1 .. repetitions]
    concurrents <- take 6 $ iterate (*2) threads

    let signature = show concurrents ++ "-" ++ show rep

    return ExperimentSetup
        { memaslapFlags = MemaslapFlags
            { msServers = servers
            , msThreads = threads
            , msConcurrency = concurrents
            , msOverwrite = 1
            , msStatFreq = Seconds runtime
            , msTime = Seconds runtime
            , msConfigFile = remoteMemaslapCfg
            }
        , localLog = "tmp/baselinelog-" ++ signature
        , remoteLog = "/tmp/baselinelog-" ++ signature
        , resultsFile = "tmp/baselinetmpresults-" ++ signature
        }

baselineExperiment :: Rules ()
baselineExperiment = do
    phony baselineExperimentRule $ need [csvOut]

    let experiments = baselineExperiments

    let resultsFiles = map resultsFile experiments
    resultsFiles &%> \_ -> do
        let localhost = RemoteLogin "syd" "localhost"
        let client = localhost
        let server = localhost

        -- Copy memaslap and its config to the client
        rsyncTo client memaslapBin tmpMemaslap
        rsyncTo client memaslapConfigFile remoteMemaslapCfg

        -- Copy memcached and its config to the server
        rsyncTo server memcachedBin tmpMemcached

        -- Start memcached on the server
        scriptAt server startMemcachedScript

        -- Wait for the server to get started
        wait 1

        -- Run all the experiments one by one
        forM_ experiments $ \ExperimentSetup{..} -> do
            let memaslapScript = script
                    [ unwords
                    $ tmpMemaslap : memaslapArgs memaslapFlags
                        ++ [">", remoteLog, "2>&1", "&"]
                    ]

            scriptAt client memaslapScript

            -- Wait long enough to be sure that memaslap is fully done.
            wait (toSeconds (msTime memaslapFlags) + 5)

            rsyncFrom client remoteLog localLog

            experimentLog <- liftIO $ readFile localLog
            case parseLog experimentLog of
                Nothing -> fail $ "could not parse logfile: " ++ localLog
                Just parsedLog -> do
                    let results = ExperimentResults
                            { erMemaslapLog = parsedLog
                            , erMemaslapFlags = memaslapFlags
                            }
                    liftIO $ LB.writeFile resultsFile $ A.encode results

        -- Make sure no memcached servers are running anymore
        overSsh server $ unwords ["killall", memcachedBinName]

    csvOut %>  \_ -> do
        need resultsFiles
        explogs <- liftIO $ mapM LB.readFile resultsFiles
        case mapM A.decode' explogs of
            Nothing -> fail "Could not parse result files."
            Just results -> liftIO $ LB.writeFile csvOut $ resultsCsv results

resultsCsv :: [ExperimentResults] -> LB.ByteString
resultsCsv = encodeByName $ header
    [ "nrServers"
    , "threads"
    , "concurrency"
    , "overwrite"
    , "time"
    , "avg"
    , "std"
    , "tps"
    ]

instance ToNamedRecord ExperimentResults where
    toNamedRecord ExperimentResults{..} =
        let MemaslapFlags{..} = erMemaslapFlags
            MemaslapLog{..} = erMemaslapLog
        in namedRecord
            [ "nrServers" .= length msServers
            , "threads" .= msThreads
            , "concurrency" .= msConcurrency
            , "overwrite" .= msOverwrite
            , "time" .= msTime
            , "avg" .= avg
            , "std" .= std
            , "tps" .= tps
            ]

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
