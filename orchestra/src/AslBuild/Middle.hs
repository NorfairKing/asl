{-# LANGUAGE RecordWildCards #-}

module AslBuild.Middle
    ( module AslBuild.Middle
    , module AslBuild.Middle.Types
    ) where

import Control.Monad
import System.Process

import Development.Shake

import AslBuild.CommonActions
import AslBuild.Constants
import AslBuild.Middle.Types
import AslBuild.Middleware
import AslBuild.Types

startMiddleOn
    :: CmdResult r
    => MiddleSetup -> Action r
startMiddleOn MiddleSetup {..} =
    scriptAt mRemoteLogin $
    script
        [ "shopt -s huponexit" -- Detach process when this script dies.
        , unwords $ middlewareCmds remoteMiddleware mMiddlewareFlags
        ]

shutdownMiddle :: MiddleSetup -> ProcessHandle -> Action ()
shutdownMiddle MiddleSetup {..} middlePh = do
    unit $
        scriptAt mRemoteLogin $
        namedScript "kill-middle" ["kill `jps | grep \"asl.jar\" | cut -d \" \" -f 1`"]
    void $ liftIO $ waitForProcess middlePh
    -- TODO check if middleware has failed

copyMiddleTraceBack :: MiddleSetup -> Action ()
copyMiddleTraceBack MiddleSetup {..} =
    rsyncFrom mRemoteLogin (mwTraceFile mMiddlewareFlags) mLocalTrace

middleRemoteServer :: MiddleSetup -> RemoteServerUrl
middleRemoteServer MiddleSetup {..} =
    RemoteServerUrl (mwIp mMiddlewareFlags) (mwPort mMiddlewareFlags)
