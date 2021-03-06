{-# LANGUAGE RecordWildCards #-}

module AslBuild.LocalMiddlewareTest
    ( module AslBuild.LocalMiddlewareTest
    , module AslBuild.LocalMiddlewareTest.Types
    ) where

import Control.Monad

import System.Exit
import System.Process

import Development.Shake

import AslBuild.CommonActions
import AslBuild.LocalMiddlewareTest.Types
import AslBuild.Memaslap
import AslBuild.Memcached
import AslBuild.Middleware
import AslBuild.Utils

runLocalMiddlewareTests :: [LocalMiddlewareTestSetup] -> Action ()
runLocalMiddlewareTests setups =
    forM_ (indexed setups) $ \(ix, setup) -> do
        putLoud $ "Running middleware test: [" ++ show ix ++ "/" ++ show (length setups) ++ "]"
        let remaining = sum $ map (runtime . snd) $ filter ((>= ix) . fst) $ indexed setups
        putLoud $ "Approximately " ++ toClockString remaining ++ " remaining."
        runLocalMiddlewareTest setup

runLocalMiddlewareTest :: LocalMiddlewareTestSetup -> Action ()
runLocalMiddlewareTest LocalMiddlewareTestSetup {..} = do
    forM_ clientSetups $ \mss -> writeMemaslapConfig (msConfigFile $ msFlags mss) $ msConfig mss
    serverPHs <- forM serverSetups runMemcachedLocally
    waitMs 250
    middlePH <- runMiddlewareLocally middlewareSetup
    waitMs 250
    clientPHs <- forM clientSetups $ runMemaslapLocally . msFlags
    let goOn = do
            wait runtime
            putLoud "Done waiting!"
    actionFinally goOn $ return ()
    forM_ (indexed clientPHs) $ \(ix, clientPH) -> do
        sec <- liftIO $ waitForProcess clientPH
        case sec of
            ExitFailure ec -> fail $ unwords ["Client", show ix, "failed with exitcode: ", show ec]
            _ -> return ()
    liftIO $ terminateProcess middlePH
    mec <- liftIO $ waitForProcess middlePH
    case mec of
        ExitFailure 143 -> return () -- Terminated by orc, good!
        ExitFailure ec -> fail $ unwords ["Middleware failed with exitcode: ", show ec]
        _ -> return ()
    liftIO $ mapM_ terminateProcess serverPHs
    forM_ (indexed serverPHs) $ \(ix, serverPH) -> do
        sec <- liftIO $ waitForProcess serverPH
        case sec of
            ExitFailure ec -> fail $ unwords ["Server", show ix, "failed with exitcode: ", show ec]
            _ -> return ()
