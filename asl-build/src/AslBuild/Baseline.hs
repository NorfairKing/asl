{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Baseline where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy       as LB
import           Data.List

import           AslBuild.Baseline.Types
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm

baselineExperimentRules :: Rules ()
baselineExperimentRules = do
    rulesForGivenBaselineExperiment localBaselineExperiment
    rulesForGivenBaselineExperiment remoteBaselineExperiment

localBaselineExperimentRule :: String
localBaselineExperimentRule = "local-baseline-experiment"

localBaselineExperiment :: BaselineExperimentRuleCfg
localBaselineExperiment = BaselineExperimentRuleCfg
    { target = localBaselineExperimentRule
    , csvOutFile = resultsDir </> "local-baseline-experiment-results.csv"
    , localLogfile = tmpDir </> "local-baseline_memaslaplog.txt"
    , baselineExperimentsCacheFile = tmpDir </> "local-baseline-experiments.json"
    , baselineLocation = BaselineLocal
    , baselineSetup = BaseLineSetup
        { repetitions = 2
        , runtime = 5
        , maxNrVirtualClients = 2
        }
    }

remoteBaselineExperimentRule :: String
remoteBaselineExperimentRule = "remote-baseline-experiment"

remoteBaselineExperiment :: BaselineExperimentRuleCfg
remoteBaselineExperiment = BaselineExperimentRuleCfg
    { target = remoteBaselineExperimentRule
    , csvOutFile = resultsDir </> "remote-baseline-experiment-results.csv"
    , localLogfile = tmpDir </> "remote-baseline_memaslaplog.txt"
    , baselineExperimentsCacheFile = tmpDir </> "remote-baseline-experiments.json"
    , baselineLocation = BaselineRemote
    , baselineSetup = BaseLineSetup
        { repetitions = 5
        , runtime = 30
        , maxNrVirtualClients = 64
        }
    }


startMemcachedScript :: Script
startMemcachedScript = Script
    "start-memcached-detached"
    [remoteMemcachedBin ++ " -d"]

localhostLogin :: RemoteLogin
localhostLogin = RemoteLogin Nothing "localhost"

rulesForGivenBaselineExperiment :: BaselineExperimentRuleCfg -> Rules ()
rulesForGivenBaselineExperiment berc@BaselineExperimentRuleCfg{..} = do
    target ~> need [csvOutFile]

    baselineExperimentsCacheFile %> \_ -> do
        (clients, server) <- case baselineLocation of
                BaselineLocal -> return (replicate 2 localhostLogin, localhostLogin)
                BaselineRemote -> (\(c,_,[s]) -> (c, s)) <$> getVms 2 0 1
        experiments <- mkBaselineExperiments berc clients server
        liftIO $ LB.writeFile baselineExperimentsCacheFile $ A.encodePretty experiments


    csvOutFile %> \_ -> do
        need [memcachedBin, memaslapBin, baselineExperimentsCacheFile]
        experiments <- do
            contents <- liftIO $ LB.readFile baselineExperimentsCacheFile
            case A.eitherDecode contents of
                Left err -> fail $ "Failed to decode contents of baselineExperimentsCacheFile: " ++ err
                Right exps -> return exps

        -- Intentionally no parallelism here.
        forM_ (indexed experiments) $ \(ix, eSetup@BaselineExperimentSetup{..}) -> do
            putLoud $ "Running experiment: [" ++ show ix ++ "/" ++ show (length experiments) ++ "]"
            let server = sRemoteLogin serverSetup

            -- Copy memcached and its config to the server
            -- Will do nothing if it's already there. Luckily
            rsyncTo server memcachedBin remoteMemcachedBin

            -- Copy memaslap and its config to the clients
            forM_ clientSetups $ \ClientSetup{..} -> do
                -- Copy the memaslap binary to the client
                rsyncTo cRemoteLogin memaslapBin remoteMemaslapBin
                -- Generate the memsalap config locally
                writeMemaslapConfig cLocalMemaslapConfigFile $ msConfig cMemaslapSettings
                -- Copy the memaslap config to the client
                rsyncTo cRemoteLogin cLocalMemaslapConfigFile $ msConfigFile $ msFlags cMemaslapSettings

            -- Start memcached on the server
            scriptAt server startMemcachedScript

            -- Wait for the server to get started
            wait 1

            -- In parallel because they have to start at the same time.
            forP_ clientSetups $ \ClientSetup{..} -> do
                let memaslapScript = script
                        [ unwords
                        $ remoteMemaslapBin : memaslapArgs (msFlags cMemaslapSettings)
                            ++ [">", cRemoteLog, "2>&1", "&"]
                        ]
                scriptAt cRemoteLogin memaslapScript

            let maxClientTime = maximum $ map (toSeconds . msTime . msFlags . cMemaslapSettings) clientSetups

            -- Wait long enough to be sure that memaslap is fully done.
            wait (maxClientTime + 5)

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
                        liftIO $ LB.writeFile cResultsFile $ A.encodePretty results

            -- Make sure no memcached servers are running anymore
            overSsh server $ unwords ["killall", memcachedBinName]

        let resultsFiles = map cResultsFile $ concatMap clientSetups experiments
        explogs <- liftIO $ mapM LB.readFile resultsFiles
        case mapM A.decode' explogs of
            Nothing -> fail "Could not parse result files."
            Just results -> liftIO $ LB.writeFile csvOutFile $ resultsCsv results


-- TODO separate login url from internal URL for cluster
mkBaselineExperiments
    :: BaselineExperimentRuleCfg
    -> [RemoteLogin] -- ^ available Clients
    -> RemoteLogin -- ^ Server
    -> Action [BaselineExperimentSetup]
mkBaselineExperiments BaselineExperimentRuleCfg{..} clientLogins serverLogin = return $ do
    let BaseLineSetup{..} = baselineSetup

    let curServerSetup = ServerSetup
            { sRemoteLogin = serverLogin
            }

    let servers = [loginToMemcachedServerUrl serverLogin]

    let threads = length servers
    concurrents <- takeWhile (<= maxNrVirtualClients) $ iterate (*2) threads
    rep <- [1 .. repetitions]

    nrClients <- [1 .. length clientLogins]

    let signature = intercalate "-"
            [ show nrClients
            , show concurrents
            , show rep
            ]
    let sign :: Int -> FilePath -> FilePath
        sign i f = f ++ "-" ++ show i ++ "-" ++ signature

    let experimentTmpDir = tmpDir </> target

    let curclients = take nrClients clientLogins
    let curClientSetups = flip map (indexed curclients) $ \(i, login) -> ClientSetup
            { cRemoteLogin = login
            , cLocalLog = sign i $ experimentTmpDir </> "baselinelog"
            , cRemoteLog = sign i $ "/tmp" </> "baselinelog" ++ "-" ++ target
            , cResultsFile = sign i $ experimentTmpDir </> "baselinetmpresults"
            , cLocalMemaslapConfigFile = sign i $ experimentTmpDir </> "distribution"
            , cMemaslapSettings = MemaslapSettings
                { msConfig = MemaslapConfig
                    { keysizeDistributions = [Distribution 128 128 1]
                    , valueDistributions = [Distribution 2048 2048 1]
                    , setProportion = 0.1
                    , getProportion = 0.9
                    }
                , msFlags = MemaslapFlags
                    { msServers = servers
                    , msThreads = threads
                    , msConcurrency = concurrents
                    , msOverwrite = 1
                    , msStatFreq = Seconds runtime
                    , msTime = Seconds runtime
                    , msConfigFile = "/tmp/memaslapcfg" ++ show i ++ "-" ++ signature
                    }
                }
            }

    return BaselineExperimentSetup
        { clientSetups = curClientSetups
        , serverSetup = curServerSetup
        }
