{-# LANGUAGE RecordWildCards #-}
module AslBuild.Client
    ( module AslBuild.Client
    , module AslBuild.Client.Types
    ) where

import           System.Directory

import           Development.Shake
import           Development.Shake.FilePath

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

startClientsOn :: [ClientSetup] -> Action ()
startClientsOn clientSetups =
    -- In parallel because they have to start at the same time.
    parScriptAt $ flip map clientSetups $ \ClientSetup{..} ->
        let line = unwords $
                remoteMemaslap : memaslapArgs (msFlags cMemaslapSettings)
                ++ [">", cRemoteLog, "2>&1", "&"]
            s = script [line]
        in (cRemoteLogin, s)


copyClientLogsBack :: [ClientSetup] -> Action ()
copyClientLogsBack clientSetups = do
    forP_ clientSetups $ \ClientSetup{..} ->
        liftIO $ createDirectoryIfMissing True $ takeDirectory cLocalLog
    phPar clientSetups $ \ClientSetup{..} ->
        rsyncFrom cRemoteLogin cRemoteLog cLocalLog

shutdownClients :: [ClientSetup] -> Action ()
shutdownClients cs = phPar cs $ \ClientSetup{..} ->
    overSsh cRemoteLogin $ unwords ["killall", "memaslap", "||", "true"]
