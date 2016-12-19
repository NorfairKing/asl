module AslBuild.LocalMiddlewareTest.Types where

import AslBuild.Memaslap
import AslBuild.Memcached
import AslBuild.Middleware

data LocalMiddlewareTestSetup = LocalMiddlewareTestSetup
    { runtime :: Int
    , clientSetups :: [ClientSetup]
    , middlewareSetup :: MiddlewareSetup
    , serverSetups :: [ServerSetup]
    } deriving (Show, Eq)

type ClientSetup = MemaslapSettings

type MiddlewareSetup = MiddlewareFlags

type ServerSetup = MemcachedFlags
