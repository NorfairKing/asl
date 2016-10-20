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

    -- Copy the memaslap config to the client
    phPar clientSetups $ \ClientSetup{..} ->
        rsyncTo cRemoteLogin cLocalMemaslapConfigFile $ msConfigFile $ msFlags cMemaslapSettings

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
