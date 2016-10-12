{-# LANGUAGE RecordWildCards #-}
module AslBuild.LocalMiddlewareTest
    ( module AslBuild.LocalMiddlewareTest
    , module AslBuild.LocalMiddlewareTest.Types
    ) where

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
    let remaining = sum
            $ map (runtime . snd)
            $ filter ((>= ix) . fst)
            $ indexed setups
    putLoud $ "Approximately " ++ toClockString remaining ++ " remaining."
    runLocalMiddlewareTest setup

runLocalMiddlewareTest :: LocalMiddlewareTestSetup -> Action ()
runLocalMiddlewareTest LocalMiddlewareTestSetup{..} = do
    need [memcachedBin, memaslapBin, outputJarFile]

    forM_ clientSetups $ \mss ->
        writeMemaslapConfig (msConfigFile $ msFlags mss) $ msConfig mss

    serverPHs <- forM serverSetups $ \mcfs ->
        cmd memcachedBin $ memcachedArgs mcfs

    waitMs 100

    middlePH <- cmd javaCmd "-jar" outputJarFile $ middlewareArgs middlewareSetup

    waitMs 100

    clientPHs <- forM clientSetups $ \mss ->
        cmd memaslapBin $ memaslapArgs $ msFlags mss

    let goOn = do
            wait runtime
            putLoud "Done waiting, killing processes!"

            forM_ (indexed serverPHs) $ \(ix, serverPH) -> do
                sec <- liftIO $ getProcessExitCode serverPH
                case sec of
                    Just (ExitFailure ec) -> fail $ unwords
                        [ "Server"
                        , show ix
                        , "failed with exitcode: "
                        , show ec
                        ]
                    _ -> return ()

            mec <- liftIO $ getProcessExitCode middlePH
            case mec of
                Just (ExitFailure ec) ->
                    fail $ "Middleware failed with exitcode: " ++ show ec
                _ -> return ()

            forM_ (indexed clientPHs) $ \(ix, clientPH) -> do
                clc <- liftIO $ getProcessExitCode clientPH
                case clc of
                    Just (ExitFailure ec) -> fail $ unwords
                        [ "Client"
                        , show ix
                        , "failed with exitcode: "
                        , show ec
                        ]
                    _ -> return ()

    actionFinally goOn $ do
        mapM_ terminateProcess clientPHs
        terminateProcess middlePH
        mapM_ terminateProcess serverPHs

        mapM_ waitForProcess clientPHs
        void $ waitForProcess middlePH
        mapM_ waitForProcess serverPHs
