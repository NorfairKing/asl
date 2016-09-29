{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.RunBaseLine where

import           Development.Shake

import           Control.Monad
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Aeson             as A
import qualified Data.ByteString.Lazy   as LB
import           Data.Csv
import           Data.List
import           GHC.Generics

import           AslBuild.CommonActions
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Types
import           AslBuild.Utils

baselineExperimentRule :: String
baselineExperimentRule = "baseline-experiment"

logFile :: FilePath
logFile = "tmp/baseline_memaslaplog.txt"

memaslapConfigFile :: FilePath
memaslapConfigFile = "tmp/distribution.txt"

csvOut :: FilePath
csvOut = "results/baseline_experiment_localhost.csv"

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
    , clientSetups  :: [ClientSetup]
    , serverSetup   :: ServerSetup
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentSetup
instance FromJSON ExperimentSetup

data ClientSetup
    = ClientSetup
    { cRemoteLogin :: RemoteLogin
    , cLocalLog    :: FilePath
    , cRemoteLog   :: FilePath
    , cResultsFile :: FilePath
    } deriving (Show, Eq, Generic)

instance ToJSON   ClientSetup
instance FromJSON ClientSetup

data ServerSetup
    = ServerSetup
    { sRemoteLogin :: RemoteLogin
    } deriving (Show, Eq, Generic)

instance ToJSON   ServerSetup
instance FromJSON ServerSetup

data ExperimentResults
    = ExperimentResults
    { erSetup       :: ExperimentSetup
    , erClientSetup :: ClientSetup
    , erMemaslapLog :: MemaslapLog
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResults
instance FromJSON ExperimentResults

data BaseLineSetup
    = BaseLineSetup
    { repetitions         :: Int
    , runtime             :: Int
    , maxNrVirtualClients :: Int
    }

baseLineSetup :: BaseLineSetup
baseLineSetup = BaseLineSetup
    { repetitions = 5
    , runtime = 30
    , maxNrVirtualClients = 64
    }

localhostname :: String
localhostname = "localhost"

loginToServerUrl :: RemoteLogin -> RemoteServerUrl
loginToServerUrl RemoteLogin{..} = RemoteServerUrl remoteHost 11211

baselineExperiments :: BaseLineSetup -> [ExperimentSetup]
baselineExperiments BaseLineSetup{..} = do
    let localhostLogin = RemoteLogin "syd" "localhost"
    let serverLogin = localhostLogin
    let clientLogin = localhostLogin

    let curServerSetup = ServerSetup
            { sRemoteLogin = serverLogin
            }


    let servers = [loginToServerUrl serverLogin]
    let threads = length servers
    concurrents <- takeWhile (<= maxNrVirtualClients) $ iterate (*2) threads
    rep <- [1 .. repetitions]


    let allClients = [clientLogin, clientLogin]

    nrClients <- [1, 2]

    let signature = intercalate "-"
            [ show nrClients
            , show concurrents
            , show rep
            ]

    let curclients = take nrClients allClients
    let curClientSetups = flip map (indexed curclients) $ \(i, login) -> ClientSetup
            { cRemoteLogin = login
            , cLocalLog = "tmp/baselinelog-" ++ show i ++ "-" ++ signature
            , cRemoteLog = "/tmp/baselinelog-" ++ show i ++ "-" ++ signature
            , cResultsFile = "tmp/baselinetmpresults-" ++ show i ++ "-" ++ signature
            }

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
        , clientSetups = curClientSetups
        , serverSetup = curServerSetup
        }

baselineExperimentRules :: Rules ()
baselineExperimentRules = do
    phony baselineExperimentRule $ need [csvOut]

    let experiments = baselineExperiments baseLineSetup

    let resultsFiles = map cResultsFile $ concatMap clientSetups experiments
    resultsFiles &%> \_ ->

        -- Intentionally no parallelism here.
        forM_ (indexed experiments) $ \(ix, eSetup@ExperimentSetup{..}) -> do
            putLoud $ "Running experiment: [" ++ show ix ++ "/" ++ show (length experiments) ++ "]"
            let server = sRemoteLogin serverSetup

            -- Copy memcached and its config to the server
            -- Will do nothing if it's already there. Luckily
            rsyncTo server memcachedBin tmpMemcached

            -- Copy memaslap and its config to the clients
            forM_ (map cRemoteLogin clientSetups) $ \clientLogin -> do
                rsyncTo clientLogin memaslapBin tmpMemaslap
                rsyncTo clientLogin memaslapConfigFile remoteMemaslapCfg

            -- Start memcached on the server
            scriptAt server startMemcachedScript

            -- Wait for the server to get started
            wait 1

            -- In parallel because they have to start at the same time.
            forP_ clientSetups $ \ClientSetup{..} -> do
                let memaslapScript = script
                        [ unwords
                        $ tmpMemaslap : memaslapArgs memaslapFlags
                            ++ [">", cRemoteLog, "2>&1", "&"]
                        ]
                scriptAt cRemoteLogin memaslapScript

            -- Wait long enough to be sure that memaslap is fully done.
            wait (toSeconds (msTime memaslapFlags) + 5)

            -- Copy the logs back here
            forP_ clientSetups $ \cSetup@ClientSetup{..} -> do
                rsyncFrom cRemoteLogin cRemoteLog cLocalLog

                experimentLog <- liftIO $ readFile cLocalLog
                case parseLog experimentLog of
                    Nothing -> fail $ "could not parse logfile: " ++ cLocalLog
                    Just parsedLog -> do
                        let results = ExperimentResults
                                { erSetup = eSetup
                                , erClientSetup = cSetup
                                , erMemaslapLog = parsedLog
                                }
                        liftIO $ LB.writeFile cResultsFile $ A.encode results

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
    [ "nrClients"
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
        let ExperimentSetup{..} = erSetup
            ClientSetup{..} = erClientSetup
            MemaslapLog{..} = erMemaslapLog
            MemaslapFlags{..} = memaslapFlags
        in namedRecord
            [ "nrClients" .= length clientSetups
            , "threads" .= msThreads
            , "concurrency" .= msConcurrency
            , "overwrite" .= msOverwrite
            , "time" .= msTime
            , "avg" .= avg
            , "std" .= std
            , "tps" .= tps
            ]
