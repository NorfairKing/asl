{-# LANGUAGE RecordWildCards #-}
module AslBuild.Memcached
    ( module AslBuild.Memcached
    , module AslBuild.Memcached.Types
    ) where

import           AslBuild.Memcached.Types

memcachedArgs :: MemcachedFlags -> [String]
memcachedArgs MemcachedFlags{..} =
    [ "-p", show memcachedPort
    , "-t", "1"
    ] ++
    [ "-d" | memcachedAsDaemon ]
