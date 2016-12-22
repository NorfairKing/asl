{-# LANGUAGE RecordWildCards #-}

module AslBuild.LocalLogTest where

import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import qualified System.Directory as D
import System.Process

import Development.Shake
import Development.Shake.FilePath

import AslBuild.BuildMemcached
import AslBuild.CommonActions
import AslBuild.Constants
import AslBuild.Memaslap
import AslBuild.Memcached
import AslBuild.Middleware
import AslBuild.Types
import AslBuild.Utils

localLogTestRule :: String
localLogTestRule = "local-log-test"

data LocalLogTestSetup = LocalLogTestSetup
    { clients :: [LocalLogClientSetup]
    , mmiddlewareFlags :: Maybe MiddlewareFlags
    , serverFlags :: MemcachedFlags
    }

data LocalLogClientSetup = LocalLogClientSetup
    { cLogFile :: FilePath
    , cSets :: MemaslapSettings
    }

localLogTestDir :: FilePath
localLogTestDir = tmpDir </> localLogTestRule

setups :: [LocalLogTestSetup]
setups = do
    workloadSecs <- [1, 2, 3]
    setProp <- [0, 0.5, 1]
    statsfreqSecs <- [1, 2]
    statsFreq <- [Nothing, Just $ Seconds statsfreqSecs]
    nrClients <- [1, 2]
    let mPort = 11210
    useMiddleware <- [True, False]
    let sign f =
            intercalate
                "-"
                [ f
                , show nrClients
                , show workloadSecs
                , show setProp
                , show statsfreqSecs
                , show $ isJust statsFreq
                , show useMiddleware
                ]
    let sfs = MemcachedFlags {memcachedPort = defaultMemcachedPort, memcachedAsDaemon = False}
    let mw =
            MiddlewareFlags
            { mwIp = localhostIp
            , mwPort = mPort
            , mwNrThreads = 1
            , mwReplicationFactor = 1
            , mwServers = [RemoteServerUrl localhostIp defaultMemcachedPort]
            , mwVerbosity = LogOff
            , mwTraceFile = localLogTestDir </> sign "middleware-trace" <.> csvExt
            , mwReadSampleRate = Nothing
            , mwWriteSampleRate = Nothing
            }
    let mmflags =
            if useMiddleware
                then Just mw
                else Nothing
    let cs =
            flip map [1 .. nrClients] $ \ix ->
                let signClient f = sign f ++ "-" ++ show (ix :: Int)
                in LocalLogClientSetup
                   { cLogFile = localLogTestDir </> signClient "local-logfile-test-log"
                   , cSets =
                         MemaslapSettings
                         { msFlags =
                               MemaslapFlags
                               { msServers =
                                     case mmflags of
                                         Nothing ->
                                             [RemoteServerUrl localhostIp $ memcachedPort sfs]
                                         Just mflags ->
                                             [RemoteServerUrl localhostIp $ mwPort mflags]
                               , msThreads = 1
                               , msConcurrency = 32
                               , msOverwrite = 0.9
                               , msWorkload = WorkFor $ Seconds workloadSecs
                               , msStatFreq = statsFreq
                               , msWindowSize = Kilo 1
                               , msConfigFile =
                                     localLogTestDir </>
                                     signClient "local-logfile-test-memaslap-cfg"
                               }
                         , msConfig =
                               MemaslapConfig
                               { keysizeDistributions = [Distribution 16 16 1]
                               , valueDistributions = [Distribution 128 128 1]
                               , setProportion = setProp
                               }
                         }
                   }
    return LocalLogTestSetup {clients = cs, mmiddlewareFlags = mmflags, serverFlags = sfs}

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
    testLogFiles <-
        do dde <- liftIO $ D.doesDirectoryExist "test_resources/memaslap-logs"
           if dde
               then liftIO $ listDirAbs "test_resources/memaslap-logs"
               else return []
    forM_ testLogFiles $ \file ->
        regressionLogTestTarget file ~> do
            ml <- parseLog file
            case ml of
                Nothing -> fail $ "Could not parse logfile " ++ file
                Just _ ->
                    putLoud $
                    "Log regression test " ++ show file ++ " completed without parse errors."
    regressionLogTestRule ~> need (map regressionLogTestTarget testLogFiles)
    localLogTestRule ~>
        need
            (map (logTestTarget . fst) (indexed setups) ++ map regressionLogTestTarget testLogFiles)
    -- Only one running at a time, multiple may parse at a time though.
    runLock <- newResource "runLock" 1
    forM_ (indexed setups) $ \(ix, LocalLogTestSetup {..}) -> do
        map cLogFile clients &%> \_
            -- Write the config to a file
         -> do
            forP_ (map cSets clients) $ \MemaslapSettings {..} ->
                writeMemaslapConfig (msConfigFile msFlags) msConfig
            withResource runLock 1 $
                -- Start memcached locally
             do
                serverPh <- runMemcachedLocally_ serverFlags
                mmiddlePh <-
                    case mmiddlewareFlags of
                        Nothing -> return Nothing
                        Just middlewareFlags ->
                            (Just <$>) $ do
                                waitMs 250
                        -- Start the middleware locally
                                runMiddlewareLocally_ middlewareFlags
                waitMs 250
                -- Run memaslap locally
                let runMemaslaps :: Action ()
                    runMemaslaps = do
                        clientPhs <-
                            forM clients $ \LocalLogClientSetup {..} -> do
                                let MemaslapSettings {..} = cSets
                                command [FileStdout cLogFile] memaslapBin $ memaslapArgs msFlags
                        forM_ clientPhs $ liftIO . waitForProcess
                -- Make sure to stop memcached
                actionFinally runMemaslaps $ do
                    case mmiddlePh of
                        Nothing -> return ()
                        Just middlePh -> do
                            terminateProcess middlePh
                            void $ waitForProcess middlePh
                    terminateProcess serverPh
                    void $ waitForProcess serverPh
        phony (logTestTarget ix) $
            forM_ (indexed $ map cLogFile clients) $ \(cix, logFile) -> do
                ml <- parseLog logFile
                case ml of
                    Nothing -> fail $ "Could not parse logfile " ++ logFile
                    Just _ ->
                        putLoud $
                        "Log test " ++ show (ix, cix) ++ " completed without parse errors."
