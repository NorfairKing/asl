{-# LANGUAGE RecordWildCards #-}
module AslBuild.Client
    ( module AslBuild.Client
    , module AslBuild.Client.Types
    ) where

import           Control.Concurrent
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
import           AslBuild.Experiment.Types
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
        let s = script
                [ "mkdir -p " ++ takeDirectory cRemoteLog
                , unwords $
                    remoteMemaslap : memaslapArgs (msFlags cMemaslapSettings)
                    ++ [">", cRemoteLog, "2>&1"]
                ]
        in (cRemoteLogin, s)


copyClientLogsBack :: [ClientSetup] -> Action ()
copyClientLogsBack clientSetups = do
    forP_ clientSetups $ \ClientSetup{..} ->
        liftIO $ createDirectoryIfMissing True $ takeDirectory cLocalLog
    phPar clientSetups $ \ClientSetup{..} ->
        rsyncFrom cRemoteLogin cRemoteLog cLocalLog

shutdownClients :: [ClientSetup] -> Action ()
shutdownClients cs = phPar (nub $ map cRemoteLogin cs) $ \cRemoteLogin ->
    scriptAt cRemoteLogin $ namedScript "kill-memaslap" [unwords ["killall", "memaslap", "||", "true"]]


waitAndWaitForClients :: TimeUnit -> [ProcessHandle] -> Action ExperimentSuccess
waitAndWaitForClients runtime clientPhs = do
    mes <- waitNicelyWith checkClients $ toSeconds runtime
    case mes of
        Just e -> return e
        Nothing -> do
            putLoud "Done waiting for the runtime, now just waiting for the clients to finish."
            waitForClients clientPhs
  where
    checkClients = do
        mecs <- forP clientPhs $ liftIO . getProcessExitCode
        pure $ case catMaybes $ checkAnyFailure mecs of
            [] -> Nothing -- Nothing went wrong
            fs -> Just $ makeClientFailure fs



waitNicelyWith :: Action (Maybe a) -> Int -> Action (Maybe a)
waitNicelyWith func is = do
    putLoud $ "Waiting for " ++ toClockString is
    go is
  where
    period = 10
    go s = do
        putLoud $ toClockString s ++ " remaining."
        mr <- func
        case mr of
            Nothing ->
                if s < period
                then do
                    liftIO $ threadDelay $ s * 1000 * 1000
                    return Nothing
                else do
                    liftIO $ threadDelay $ period * 1000 * 1000
                    go $ s - period
            r -> return r

checkAnyFailure :: [Maybe ExitCode] -> [Maybe String]
checkAnyFailure mecs = flip map (indexed mecs) $ \(cix, mec) ->
    case mec of
        Nothing -> Nothing
        Just ExitSuccess -> Nothing
        Just (ExitFailure e) -> Just $ unwords
            [ "Client"
            , show cix
            , "failed with exit code"
            , show e
            ]

makeClientFailure :: [String] -> ExperimentSuccess
makeClientFailure [] = ExperimentSuccess
makeClientFailure fails = ExperimentFailure $ show fails

waitForClients :: [ProcessHandle] -> Action ExperimentSuccess
waitForClients phs = go
  where
    go = do
        mecs <- forP phs $ liftIO . getProcessExitCode
        printMecs mecs
        let fails = checkAnyFailure mecs
        case catMaybes fails of
            [] ->
                if all isJust mecs
                then return ExperimentSuccess
                else do
                    void $ liftIO $
                        timeout (5 * 1000 * 1000) $
                            forM_ phs waitForProcess
                    go
            fs -> return $ makeClientFailure fs

    printMecs mecs =
        unless (all isNothing mecs) $
            putLoud $ unwords $ map (\mec -> "[" ++ showMec mec ++ "]") mecs
    showMec Nothing = " "
    showMec (Just ExitSuccess) = "✓"
    showMec (Just (ExitFailure _)) = "X"
