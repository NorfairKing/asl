{-# LANGUAGE RecordWildCards #-}
module AslBuild.Memcached
    ( module AslBuild.Memcached
    , module AslBuild.Memcached.Types
    ) where

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.Memcached.Types

runMemcachedLocally :: CmdResult r => MemcachedFlags -> Action r
runMemcachedLocally flags = do
    need [memcachedBin]
    cmd $ memcachedCmds memcachedBin flags

memcachedCmds :: FilePath -> MemcachedFlags -> [String]
memcachedCmds path flags = path : memcachedArgs flags

memcachedArgs :: MemcachedFlags -> [String]
memcachedArgs MemcachedFlags{..} =
    [ "-p", show memcachedPort
    , "-t", "1"
    ] ++
    [ "-d" | memcachedAsDaemon ]
