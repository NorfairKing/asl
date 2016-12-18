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

startServersOn :: [ServerSetup] -> Action ()
startServersOn = parScriptAt . map (\ServerSetup{..} -> (sRemoteLogin, script
    [ unwords $ remoteMemcached : memcachedArgs sMemcachedFlags
    ]))

shutdownServers :: [ServerSetup] -> Action ()
shutdownServers sss = phPar (nub $ map sRemoteLogin sss) $ \rl ->
    overSsh rl $ unwords ["killall", remoteMemcached]
