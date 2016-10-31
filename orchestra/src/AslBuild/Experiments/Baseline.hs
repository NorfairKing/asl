{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Experiments.Baseline
    ( module AslBuild.Experiments.Baseline
    , module AslBuild.Experiments.Baseline.Types
    ) where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad
import qualified Data.Aeson                          as A
import qualified Data.Aeson.Encode.Pretty            as A
import qualified Data.ByteString.Lazy                as LB
import           Data.List

import           AslBuild.BuildMemcached
import           AslBuild.Client
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiments.Baseline.Types
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Provision
import           AslBuild.Server
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm

baselineExperimentRules :: Rules ()
baselineExperimentRules = do
    rulesForGivenBaselineExperiment smallLocalBaselineExperiment
    rulesForGivenBaselineExperiment localBaselineExperiment
    -- rulesForGivenBaselineExperiment bigLocalBaselineExperiment
    rulesForGivenBaselineExperiment smallRemoteBaselineExperiment
    rulesForGivenBaselineExperiment remoteBaselineExperiment

smallLocalBaselineExperimentRule :: String
smallLocalBaselineExperimentRule = "small-local-baseline-experiment"

smallLocalBaselineExperiment :: BaselineExperimentRuleCfg
smallLocalBaselineExperiment = BaselineExperimentRuleCfg
    { target = smallLocalBaselineExperimentRule
    , csvOutFile = tmpDir
        </> smallLocalBaselineExperimentRule </> "results"
        </> "small-local-baseline-experiment-results.csv"
    , localLogfile = tmpDir </> "small-local-baseline_memaslaplog.txt"
    , maxNrClients = 2
    , baselineExperimentsCacheFile = tmpDir </> "small-local-baseline-experiments.json"
    , baselineLocation = BaselineLocal
    , baselineSetup = BaseLineSetup
        { repetitions = 1
        , runtime = 2
        , maxNrVirtualClients = 2
        }
    }

localBaselineExperimentRule :: String
localBaselineExperimentRule = "local-baseline-experiment"

localBaselineExperiment :: BaselineExperimentRuleCfg
localBaselineExperiment = BaselineExperimentRuleCfg
    { target = localBaselineExperimentRule
    , csvOutFile = resultsDir </> "local-baseline-experiment-results.csv"
    , localLogfile = tmpDir </> "local-baseline_memaslaplog.txt"
    , maxNrClients = maxNrClients remoteBaselineExperiment
    , baselineExperimentsCacheFile = tmpDir </> "local-baseline-experiments.json"
    , baselineLocation = BaselineLocal
    , baselineSetup = baselineSetup remoteBaselineExperiment
    }

bigLocalBaselineExperimentRule :: String
bigLocalBaselineExperimentRule = "big-local-baseline-experiment"

bigLocalBaselineExperiment :: BaselineExperimentRuleCfg
bigLocalBaselineExperiment = BaselineExperimentRuleCfg
    { target = bigLocalBaselineExperimentRule
    , csvOutFile = resultsDir </> "big-local-baseline-experiment-results.csv"
    , localLogfile = tmpDir </> "big-local-baseline_memaslaplog.txt"
    , maxNrClients = 2
    , baselineExperimentsCacheFile = tmpDir </> "big-local-baseline-experiments.json"
    , baselineLocation = BaselineLocal
    , baselineSetup = BaseLineSetup
        { repetitions = 25
        , runtime = 60
        , maxNrVirtualClients = 128
        }
    }

smallRemoteBaselineExperimentRule :: String
smallRemoteBaselineExperimentRule = "small-remote-baseline-experiment"

smallRemoteBaselineExperiment :: BaselineExperimentRuleCfg
smallRemoteBaselineExperiment = BaselineExperimentRuleCfg
    { target = smallRemoteBaselineExperimentRule
    , csvOutFile = resultsDir </> "small-remote-baseline-experiment-results.csv"
    , localLogfile = tmpDir </> "small-remote-baseline_memaslaplog.txt"
    , maxNrClients = 2
    , baselineExperimentsCacheFile = tmpDir </> "small-remote-baseline-experiments.json"
    , baselineLocation = BaselineLocal -- BaselineRemote
    , baselineSetup = BaseLineSetup
        { repetitions = 1
        , runtime = 5
        , maxNrVirtualClients = 2 -- 4
        }
    }

remoteBaselineExperimentRule :: String
remoteBaselineExperimentRule = "remote-baseline-experiment"

remoteBaselineExperiment :: BaselineExperimentRuleCfg
remoteBaselineExperiment = BaselineExperimentRuleCfg
    { target = remoteBaselineExperimentRule
    , csvOutFile = resultsDir </> "remote-baseline-experiment-results.csv"
    , localLogfile = tmpDir </> "remote-baseline_memaslaplog.txt"
    , maxNrClients = 2
    , baselineExperimentsCacheFile = tmpDir </> "remote-baseline-experiments.json"
    , baselineLocation = BaselineLocal -- BaselineRemote
    , baselineSetup = BaseLineSetup
        { repetitions = 1 -- 5
        , runtime = 5 -- 30
        , maxNrVirtualClients = 2 -- 128
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

        (experiments, vmsNeeded) <- do
            contents <- liftIO $ LB.readFile baselineExperimentsCacheFile
            case A.eitherDecode contents of
                Left err -> fail $ "Failed to decode contents of baselineExperimentsCacheFile: " ++ err
                Right exps -> return exps

        need [provisionLocalhostRule]
        -- startVms vmsNeeded
        provisionVmsFromData vmsNeeded

        -- Intentionally no parallelism here.
        -- We need to do experiments one at a time.
        forM_ (indexed experiments) $ \(ix, eSetup@BaselineExperimentSetup{..}) -> do
            let nrOfExperiments = length experiments
            putLoud $ "Running experiment: [" ++ show ix ++ "/" ++ show nrOfExperiments ++ "]"
            let maxClientTime = maximum $ map (toSeconds . msTimeUnsafe . msWorkload . msFlags . cMemaslapSettings) clientSetups
            putLoud $ "Approximately " ++ toClockString ((1 + 5 + maxClientTime) * (nrOfExperiments - ix)) ++ " remaining."

            -- Set up the memaslap configs on the clients
            setupClientConfigs clientSetups

            let ServerSetup{..} = serverSetup

            -- Start memcached on the server
            startServersOn [serverSetup]

            -- Wait for the server to get started
            waitMs 250

            -- Start the client memaslaps
            startClientsOn clientSetups

            -- Wait long enough to be sure that memaslap is fully done.
            wait (maxClientTime + 5)

            -- Copy the logs back here
            copyClientLogsBack clientSetups

            -- Prepare the analysis files
            forP_ clientSetups $ \cSetup@ClientSetup{..} -> do
                mel <- parseLog cLocalLog
                case mel of
                    Nothing -> fail $ "could not parse logfile: " ++ cLocalLog
                    Just parsedLog -> do
                        let results = ExperimentResults
                                { erSetup = eSetup
                                , erClientSetup = cSetup
                                , erMemaslapLog = parsedLog
                                , erClientIndex = cIndex
                                }
                        liftIO $ LB.writeFile cResultsFile $ A.encodePretty results
                        case makeLogLine results of
                            Just _ -> return ()
                            Nothing -> fail $ unwords
                                [ "Could not make log line from experiment results"
                                , cResultsFile
                                , ", total stats missing?"
                                ]

            -- Make sure no memcached servers are running anymore
            shutdownServers [serverSetup]

        -- stopVms vmsNeeded

        let resultsFiles = map cResultsFile $ concatMap clientSetups experiments
        explogs <- liftIO $ mapM LB.readFile resultsFiles
        case mapM (makeLogLine <=< A.decode') explogs of
            Nothing -> fail "Could not parse result files."
            Just results -> liftIO $ LB.writeFile csvOutFile $ resultsCsv results


mkBaselineExperiments :: BaselineExperimentRuleCfg -> Action ([BaselineExperimentSetup], [VmData])
mkBaselineExperiments BaselineExperimentRuleCfg{..} = do
    (clientLogins, memcachedServer, serverSetup, remoteVmsNeeded) <- case baselineLocation of
        BaselineLocal -> do
            let l = "localhost"
                p = 11211
                c = replicate maxNrClients $ RemoteLogin Nothing l
                m = RemoteServerUrl l p
                s = ServerSetup
                    { sRemoteLogin = RemoteLogin Nothing l
                    , sMemcachedFlags = MemcachedFlags
                        { memcachedPort = p
                        , memcachedAsDaemon = True
                        }
                    , sIndex = 0
                    }
            return (c, m, s, [])
        BaselineRemote -> do
            (cs, s) <- (\(c,_,[s]) -> (c, s)) <$> getVms maxNrClients 0 1
            let p = 11211
                c_ = map (\vm -> RemoteLogin (Just $ vmAdmin vm) (vmFullUrl vm)) cs
                m_ = RemoteServerUrl (vmPrivateIp s) p
                s_ = ServerSetup
                    { sRemoteLogin = RemoteLogin (Just $ vmAdmin s) (vmFullUrl s)
                    , sMemcachedFlags = MemcachedFlags
                        { memcachedPort = p
                        , memcachedAsDaemon = True
                        }
                    , sIndex = 0
                    }
            return (c_, m_, s_, s:cs)
    return $ (\exps -> (exps, remoteVmsNeeded)) $ do
        let BaseLineSetup{..} = baselineSetup

        let servers = [memcachedServer]

        nrClients <- [1 .. length clientLogins]

        let steps = max (maxNrVirtualClients `div` (12 * nrClients)) 1
        concurrents <- (++ [maxNrVirtualClients `div` nrClients]) $ takeWhile (< (maxNrVirtualClients `div` nrClients)) $ iterate (+ steps) 1
        rep <- [1 .. repetitions]


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
                        { keysizeDistributions = [Distribution 16 16 1]
                        , valueDistributions = [Distribution 128 128 1]
                        , setProportion = 0.01
                        , getProportion = 0.99
                        }
                    , msFlags = MemaslapFlags
                        { msServers = servers
                        , msThreads = 1
                        , msConcurrency = concurrents
                        , msOverwrite = 0.9
                        , msStatFreq = Just $ Seconds runtime
                        , msWorkload = WorkFor $ Seconds runtime
                        , msConfigFile = sign i "/tmp/memaslapcfg"
                        }
                    }
                }

        return BaselineExperimentSetup
            { repetition = rep
            , clientSetups = curClientSetups
            , serverSetup = serverSetup
            }
