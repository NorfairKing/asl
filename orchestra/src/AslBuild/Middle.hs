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
    [ "shopt -s huponexit" -- Terminate process when this script dies.
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
