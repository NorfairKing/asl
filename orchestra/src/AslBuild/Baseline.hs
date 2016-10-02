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
import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm
import           AslBuild.Vm.Types

baselineExperimentRules :: Rules ()
baselineExperimentRules = do
    rulesForGivenBaselineExperiment localBaselineExperiment
    rulesForGivenBaselineExperiment bigLocalBaselineExperiment
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
        , runtime = 2
        , maxNrVirtualClients = 2
        }
    }

bigLocalBaselineExperimentRule :: String
bigLocalBaselineExperimentRule = "big-local-baseline-experiment"

bigLocalBaselineExperiment :: BaselineExperimentRuleCfg
bigLocalBaselineExperiment = BaselineExperimentRuleCfg
    { target = bigLocalBaselineExperimentRule
    , csvOutFile = resultsDir </> "big-local-baseline-experiment-results.csv"
    , localLogfile = tmpDir </> "big-local-baseline_memaslaplog.txt"
    , baselineExperimentsCacheFile = tmpDir </> "big-local-baseline-experiments.json"
    , baselineLocation = BaselineLocal
    , baselineSetup = BaseLineSetup
        { repetitions = 25
        , runtime = 60
        , maxNrVirtualClients = 128
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


localhostLogin :: RemoteLogin
localhostLogin = RemoteLogin Nothing "localhost"

rulesForGivenBaselineExperiment :: BaselineExperimentRuleCfg -> Rules ()
rulesForGivenBaselineExperiment berc@BaselineExperimentRuleCfg{..} = do
    target ~> need [csvOutFile]

    baselineExperimentsCacheFile %> \_ -> do
        experiments <- mkBaselineExperiments berc
        liftIO $ LB.writeFile baselineExperimentsCacheFile $ A.encodePretty experiments

    csvOutFile %> \_ -> do
        need [memcachedBin, memaslapBin, baselineExperimentsCacheFile]
        experiments <- do
            contents <- liftIO $ LB.readFile baselineExperimentsCacheFile
            case A.eitherDecode contents of
                Left err -> fail $ "Failed to decode contents of baselineExperimentsCacheFile: " ++ err
                Right exps -> return exps

        -- Get the clients configs set up
        let allClientSetups = nub $ concatMap clientSetups experiments
        forP_ allClientSetups $ \ClientSetup{..} -> do
            -- Generate the memsalap config locally
            writeMemaslapConfig cLocalMemaslapConfigFile $ msConfig cMemaslapSettings

            -- Copy the memaslap config to the client
            rsyncTo cRemoteLogin cLocalMemaslapConfigFile $ msConfigFile $ msFlags cMemaslapSettings

        -- Intentionally no parallelism here.
        -- We need to do experiments one at a time.
        forM_ (indexed experiments) $ \(ix, eSetup@BaselineExperimentSetup{..}) -> do
            let nrOfExperiments = length experiments
            putLoud $ "Running experiment: [" ++ show ix ++ "/" ++ show nrOfExperiments ++ "]"
            let maxClientTime = maximum $ map (toSeconds . msTime . msFlags . cMemaslapSettings) clientSetups
            putLoud $ "Approximately " ++ toClockString ((1 + 5 + maxClientTime) * (nrOfExperiments - ix)) ++ " remaining."

            let ServerSetup{..} = serverSetup

            -- Start memcached on the server
            scriptAt sRemoteLogin $ script
                [ unwords $ remoteMemcachedBin : memcachedArgs sMemcachedFlags
                ]

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
                                , erClientIndex = cIndex
                                }
                        liftIO $ LB.writeFile cResultsFile $ A.encodePretty results

            -- Make sure no memcached servers are running anymore
            overSsh sRemoteLogin $ unwords ["killall", memcachedBinName]

        let resultsFiles = map cResultsFile $ concatMap clientSetups experiments
        explogs <- liftIO $ mapM LB.readFile resultsFiles
        case mapM A.decode' explogs of
            Nothing -> fail "Could not parse result files."
            Just results -> liftIO $ LB.writeFile csvOutFile $ resultsCsv results


mkBaselineExperiments :: BaselineExperimentRuleCfg -> Action [BaselineExperimentSetup]
mkBaselineExperiments BaselineExperimentRuleCfg{..} = do
    (clientLogins, memcachedServer, serverSetup) <- case baselineLocation of
        BaselineLocal -> do
            let l = "localhost"
                p = 11211
                c = replicate 2 $ RemoteLogin Nothing l
                m = RemoteServerUrl l p
                s = ServerSetup (RemoteLogin Nothing l) $ MemcachedFlags
                    { memcachedPort = p
                    , memcachedAsDaemon = True
                    }
            return (c, m, s)
        BaselineRemote -> do
            (cs, s) <- (\(c,_,[s]) -> (c, s)) <$> getVms 2 0 1
            let p = 11211
                c_ = map (\vm -> RemoteLogin (Just $ vmAdmin vm) (vmFullUrl vm)) cs
                m_ = RemoteServerUrl (vmPrivateIp s) p
                s_ = ServerSetup (RemoteLogin (Just $ vmAdmin s) (vmFullUrl s)) $ MemcachedFlags
                    { memcachedPort = p
                    , memcachedAsDaemon = True
                    }
            return (c_, m_, s_)
    return $ do
        let BaseLineSetup{..} = baselineSetup

        let servers = [memcachedServer]
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
        let curClientSetups = flip map (indexed curclients) $ \(i, clientLogin) -> ClientSetup
                { cRemoteLogin = clientLogin
                , cLocalLog = sign i $ experimentTmpDir </> "baselinelog"
                , cRemoteLog = sign i $ "/tmp" </> "baselinelog" ++ "-" ++ target
                , cResultsFile = sign i $ experimentTmpDir </> "baselinetmpresults"
                , cLocalMemaslapConfigFile = sign i $ experimentTmpDir </> "distribution"
                , cIndex = i
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
                        , msConfigFile = sign i "/tmp/memaslapcfg"
                        }
                    }
                }

        return BaselineExperimentSetup
            { clientSetups = curClientSetups
            , serverSetup = serverSetup
            }
