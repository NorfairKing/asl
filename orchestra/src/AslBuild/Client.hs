{-# LANGUAGE RecordWildCards #-}
module AslBuild.Client
    ( module AslBuild.Client
    , module AslBuild.Client.Types
    ) where

import           Development.Shake

import           AslBuild.Client.Types
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memaslap
import           AslBuild.Types
import           AslBuild.Utils

setupClientConfigs :: [ClientSetup] -> Action ()
setupClientConfigs clientSetups = forP_ clientSetups $ \ClientSetup{..} -> do
    -- Generate the memsalap config locally
    writeMemaslapConfig cLocalMemaslapConfigFile $ msConfig cMemaslapSettings

    -- Copy the memaslap config to the client
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
copyClientLogsBack clientSetups = phPar clientSetups $ \ClientSetup{..} ->
    rsyncFrom cRemoteLogin cRemoteLog cLocalLog
