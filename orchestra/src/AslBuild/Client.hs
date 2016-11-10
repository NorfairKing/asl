{-# LANGUAGE RecordWildCards #-}
module AslBuild.Client
    ( module AslBuild.Client
    , module AslBuild.Client.Types
    ) where

import           Control.Monad
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.Timeout

import           Development.Shake
import           Development.Shake.FilePath

import           System.Exit
import           System.Process

import           AslBuild.Client.Types
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memaslap
import           AslBuild.Types
import           AslBuild.Utils

setupClientConfigs :: [ClientSetup] -> Action ()
setupClientConfigs clientSetups = do
    -- Generate the memsalap config locally
    forP_ clientSetups $ \ClientSetup{..} ->
        writeMemaslapConfig cLocalMemaslapConfigFile $ msConfig cMemaslapSettings

    let remoteConfigFile c = msConfigFile $ msFlags $ cMemaslapSettings c

    -- Create the directory that the log will be in
    phPar clientSetups $ \c@ClientSetup{..} ->
        overSsh cRemoteLogin $ "mkdir --parents " ++ takeDirectory (remoteConfigFile c)

    -- Copy the memaslap config to the client
    phPar clientSetups $ \c@ClientSetup{..} ->
        rsyncTo cRemoteLogin cLocalMemaslapConfigFile $ remoteConfigFile c

startClientsOn :: [ClientSetup] -> Action [ProcessHandle]
startClientsOn clientSetups =
    parScriptAtResult $ flip map clientSetups $ \ClientSetup{..} ->
        let line = unwords $
                remoteMemaslap : memaslapArgs (msFlags cMemaslapSettings)
                ++ [">", cRemoteLog, "2>&1"]
            s = script [line]
        in (cRemoteLogin, s)


copyClientLogsBack :: [ClientSetup] -> Action ()
copyClientLogsBack clientSetups = do
    forP_ clientSetups $ \ClientSetup{..} ->
        liftIO $ createDirectoryIfMissing True $ takeDirectory cLocalLog
    phPar clientSetups $ \ClientSetup{..} ->
        rsyncFrom cRemoteLogin cRemoteLog cLocalLog

shutdownClients :: [ClientSetup] -> Action ()
shutdownClients cs = phPar (nub $ map cRemoteLogin cs) $ \cRemoteLogin ->
    scriptAt cRemoteLogin $ script [unwords ["killall", "memaslap", "||", "true"]]

waitForClients :: [ProcessHandle] -> Action ()
waitForClients phs = go
  where
    go = do
        mecs <- forP phs $ liftIO . getProcessExitCode
        printMecs mecs
        checkAnyFailure mecs
        unless (all isJust mecs) $ do
            void $ liftIO $
                timeout (5 * 1000 * 1000) $
                    forM_ phs waitForProcess
            go

    checkAnyFailure mecs = forM_ (indexed mecs) $ \(cix, mec) ->
        case mec of
            Nothing -> return ()
            Just ExitSuccess -> return ()
            Just (ExitFailure e) -> fail $ unwords
                [ "Client"
                , show cix
                , "failed with exit code"
                , show e
                ]

    printMecs mecs =
        unless (all isNothing mecs) $
            putLoud $ unwords $ map (\mec -> "[" ++ showMec mec ++ "]") mecs
    showMec Nothing = " "
    showMec (Just ExitSuccess) = "✓"
    showMec (Just (ExitFailure _)) = "X"
