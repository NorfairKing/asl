{-# LANGUAGE RecordWildCards #-}
module AslBuild.Server
    ( module AslBuild.Server
    , module AslBuild.Server.Types
    ) where

import           Development.Shake

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memcached
import           AslBuild.Server.Types
import           AslBuild.Types

startServersOn :: [ServerSetup] -> Action ()
startServersOn sss = phPar sss $ \ServerSetup{..} -> scriptAt sRemoteLogin $ script
    [ unwords $ remoteMemcached : memcachedArgs sMemcachedFlags
    ]

shutdownServers :: [ServerSetup] -> Action ()
shutdownServers sss = phPar sss $ \ServerSetup{..} ->
    overSsh sRemoteLogin $ unwords ["killall", remoteMemcached]
