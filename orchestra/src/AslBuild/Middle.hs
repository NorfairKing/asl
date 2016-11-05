{-# LANGUAGE RecordWildCards #-}
module AslBuild.Middle
    ( module AslBuild.Middle
    , module AslBuild.Middle.Types
    ) where

import           Development.Shake

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Middle.Types
import           AslBuild.Middleware
import           AslBuild.Types

startMiddleOn :: CmdResult r => MiddleSetup -> Action r
startMiddleOn MiddleSetup{..} = scriptAt mRemoteLogin $ script
    [ "shopt -s huponexit" -- Detach process when this script dies.
    , unwords $
        javaCmd :
        -- ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5005"]
        ["-jar", remoteMiddleware]
        ++ middlewareArgs mMiddlewareFlags
        ++ ["2>&1", "&"]
    ]

shutdownMiddle :: MiddleSetup -> Action ()
shutdownMiddle MiddleSetup{..} = scriptAt mRemoteLogin $ script
    [ "kill `jps | grep \"asl.jar\" | cut -d \" \" -f 1`" ]

copyMiddleTraceBack :: MiddleSetup -> Action ()
copyMiddleTraceBack MiddleSetup{..} =
    rsyncFrom mRemoteLogin (mwTraceFile mMiddlewareFlags) mLocalTrace

middleRemoteServer :: MiddleSetup -> RemoteServerUrl
middleRemoteServer MiddleSetup{..} = RemoteServerUrl
    (mwIp mMiddlewareFlags)
    (mwPort mMiddlewareFlags)
