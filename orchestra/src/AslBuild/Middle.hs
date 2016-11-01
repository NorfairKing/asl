{-# LANGUAGE RecordWildCards #-}
module AslBuild.Middle
    ( module AslBuild.Middle
    , module AslBuild.Middle.Types
    ) where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiment.Types
import           AslBuild.Middle.Types
import           AslBuild.Middleware
import           AslBuild.Server.Types
import           AslBuild.Types

startMiddleOn :: CmdResult r => MiddleSetup -> Action r
startMiddleOn MiddleSetup{..} = scriptAt mRemoteLogin $ script
    [ "shopt -s huponexit" -- Detach process when this script dies.
    , unwords $
        [javaCmd, "-jar", remoteMiddleware]
        ++ middlewareArgs mMiddlewareFlags
        ++ ["&"]
    ]

shutdownMiddle :: MiddleSetup -> Action ()
shutdownMiddle MiddleSetup{..} =
    overSsh mRemoteLogin $ unwords ["killall", "java"]

copyMiddleTraceBack :: MiddleSetup -> Action ()
copyMiddleTraceBack MiddleSetup{..} =
    rsyncFrom mRemoteLogin (mwTraceFile mMiddlewareFlags) mLocalTrace

middleRemoteServer :: MiddleSetup -> RemoteServerUrl
middleRemoteServer MiddleSetup{..} = RemoteServerUrl
    (mwIp mMiddlewareFlags)
    (mwPort mMiddlewareFlags)
