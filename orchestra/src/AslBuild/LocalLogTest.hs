{-# LANGUAGE RecordWildCards #-}
module AslBuild.LocalLogTest where

import           Control.Monad
import           Data.List
import           System.Process

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memaslap
import           AslBuild.Types
import           AslBuild.Utils

localLogTestRule :: String
localLogTestRule = "local-logfile-test"

data LocalLogTestSetup
    = LocalLogTestSetup
    { logFile :: FilePath
    , mSets   :: MemaslapSettings
    }

localLogTestDir :: FilePath
localLogTestDir = tmpDir </> localLogTestRule

setups :: [LocalLogTestSetup]
setups = do
    workloadSecs <- [1, 2, 5, 10]
    statsfreqSecs <- [1, 2, 5, 10]

    let sign f = intercalate "-" [f, show workloadSecs, show statsfreqSecs]

    return LocalLogTestSetup
        { logFile = localLogTestDir </> sign "local-logfile-test-log"
        , mSets = MemaslapSettings
            { msFlags = MemaslapFlags
                { msServers = [RemoteServerUrl localhostIp defaultMemcachedPort]
                , msThreads = 1
                , msConcurrency = 64
                , msOverwrite = 0.9
                , msWorkload = WorkFor $ Seconds workloadSecs
                , msStatFreq = Just $ Seconds statsfreqSecs
                , msConfigFile = localLogTestDir </> sign "local-logfile-test-memaslap-cfg"
                }
            , msConfig = MemaslapConfig
                { keysizeDistributions = [Distribution 16 16 1]
                , valueDistributions = [Distribution 128 128 1]
                , setProportion = 0.1
                , getProportion = 0.9
                }
            }
        }

logTestTarget :: Int -> String
logTestTarget ix = localLogTestRule ++ "-" ++ show ix

localLogTestRules :: Rules ()
localLogTestRules = do
    -- Only one running at a time, multiple may parse at a time though.
    runLock <- newResource "runLock" 1
    forM_ (indexed setups) $ \(ix, LocalLogTestSetup{..}) -> do
        logFile %> \_ -> do
            need [memcachedBin, memaslapBin]

            let MemaslapSettings{..} = mSets
            -- Write the config to a file
            writeMemaslapConfig (msConfigFile msFlags) msConfig

            withResource runLock 1 $ do
                -- Start memcached locally
                ph <- cmd memcachedBin

                -- Run memaslap locally
                let runMemaslap :: Action ()
                    runMemaslap = command [FileStdout logFile] memaslapBin (memaslapArgs msFlags)

                -- Make sure to stop memcached
                actionFinally runMemaslap $ do
                    terminateProcess ph
                    void $ waitForProcess ph

        logTestTarget ix ~> do
            ml <- parseLog logFile
            case ml of
                Nothing        -> fail "Could not parse logfile."
                Just _ -> putLoud $ "Log test " ++ show ix ++ " completed without parse errors."

    localLogTestRule ~> need (map (logTestTarget . fst) $ indexed setups)
