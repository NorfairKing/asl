{-# LANGUAGE RecordWildCards #-}
module AslBuild.LocalLogTest where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Directory
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
    statsFreq <- [Nothing, Just $ Seconds statsfreqSecs]

    let sign f = intercalate "-" [f, show workloadSecs, show statsfreqSecs, show $ isJust statsFreq]

    return LocalLogTestSetup
        { logFile = localLogTestDir </> sign "local-logfile-test-log"
        , mSets = MemaslapSettings
            { msFlags = MemaslapFlags
                { msServers = [RemoteServerUrl localhostIp defaultMemcachedPort]
                , msThreads = 1
                , msConcurrency = 64
                , msOverwrite = 0.9
                , msWorkload = WorkFor $ Seconds workloadSecs
                , msStatFreq = statsFreq
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

regressionLogTestRule :: String
regressionLogTestRule = "log-regression-test"

regressionLogTestTarget :: FilePath -> String
regressionLogTestTarget file = regressionLogTestRule ++ file

localLogTestRules :: Rules ()
localLogTestRules = do
    let listDirAbs :: FilePath -> IO [FilePath]
        listDirAbs dir = map (dir </>) <$> listDirectory dir
    testLogFiles <- liftIO $ listDirAbs "test_resources/memaslap-logs"

    forM_ testLogFiles $ \file ->
        regressionLogTestTarget file ~> do
            ml <- parseLog file
            case ml of
                Nothing -> fail $ "Could not parse logfile " ++ file
                Just _ -> putLoud $ "Log regression test " ++ show file ++ " completed without parse errors."

    regressionLogTestRule ~> need (map regressionLogTestTarget testLogFiles)

    localLogTestRule ~> need
            (map (logTestTarget . fst) (indexed setups)
            ++
            map regressionLogTestTarget testLogFiles)

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
                Nothing -> fail $ "Could not parse logfile " ++ logFile
                Just _ -> putLoud $ "Log test " ++ show ix ++ " completed without parse errors."

