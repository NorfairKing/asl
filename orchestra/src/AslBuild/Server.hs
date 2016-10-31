{-# LANGUAGE RecordWildCards #-}
module AslBuild.Server
    ( module AslBuild.Server
    , module AslBuild.Server.Types
    ) where

import           Data.List

import           Development.Shake

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memcached
import           AslBuild.Server.Types
import           AslBuild.Types
import           AslBuild.Utils

startServersOn :: [ServerSetup] -> Action ()
startServersOn sss = phPar sss $ \ServerSetup{..} -> scriptAt sRemoteLogin $ script
    [ unwords $ remoteMemcached : memcachedArgs sMemcachedFlags
    ]

shutdownServers :: [ServerSetup] -> Action ()
shutdownServers sss = phPar (nub $ map sRemoteLogin sss) $ \rl ->
    overSsh rl $ unwords ["killall", remoteMemcached]

genServerSetups :: [(RemoteLogin, String)] -> [ServerSetup]
genServerSetups sers = flip map (indexed sers) $ \(six, (sLogin, _)) -> ServerSetup
    { sRemoteLogin = sLogin
    , sIndex = six
    , sMemcachedFlags = MemcachedFlags
        { memcachedPort = serverPort + six
        , memcachedAsDaemon = True
        }
    }
  where serverPort = 12345
