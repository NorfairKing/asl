{-# LANGUAGE RecordWildCards #-}
module AslBuild.LocalMiddlewareTest
    ( module AslBuild.LocalMiddlewareTest
    , module AslBuild.LocalMiddlewareTest.Types
    ) where

import           Control.Concurrent
import           Control.Monad

import           System.Exit
import           System.Process

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.LocalMiddlewareTest.Types
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Middleware
import           AslBuild.Utils

runLocalMiddlewareTests :: [LocalMiddlewareTestSetup] -> Action ()
runLocalMiddlewareTests setups = forM_ (indexed setups) $ \(ix, setup) -> do
    putLoud $ "Running middleware test: [" ++ show ix ++ "/" ++ show (length setups) ++ "]"
    runLocalMiddlewareTest setup

runLocalMiddlewareTest :: LocalMiddlewareTestSetup -> Action ()
runLocalMiddlewareTest LocalMiddlewareTestSetup{..} = do
    need [memcachedBin, memaslapBin, outputJarFile]

    forM_ clientSetups $ \mss ->
        writeMemaslapConfig (msConfigFile $ msFlags mss) $ msConfig mss

    serverPHs <- forM serverSetups $ \mcfs ->
        cmd memcachedBin $ memcachedArgs mcfs

    middlePH <- cmd javaCmd "-jar" outputJarFile $ middlewareArgs middlewareSetup

    waitMs 250

    clientPHs <- forM clientSetups $ \mss ->
        cmd memaslapBin $ memaslapArgs $ msFlags mss

    let terminateAll = do
            mapM_ terminateProcess clientPHs
            terminateProcess middlePH
            mapM_ terminateProcess serverPHs

    let goOn = do
            wait runtime
            putLoud "Done waiting, killing processes!"

            forM_ (indexed clientPHs) $ \(ix, clientPH) -> do
                clc <- liftIO $ getProcessExitCode clientPH
                case clc of
                    Just (ExitFailure ec) ->
                        fail $ "client " ++ show ix ++ " failed with exitcode: " ++ show ec
                    _ -> return ()

            mec <- liftIO $ getProcessExitCode middlePH
            case mec of
                Just (ExitFailure ec) ->
                    fail $ "Middleware failed with exitcode: " ++ show ec
                _ -> return ()

    actionFinally goOn $ do
        terminateAll
        threadDelay $ 1 * 1000 * 1000 -- Wait for everything to grind to a halt.
