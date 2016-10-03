{-# LANGUAGE DeriveGeneric #-}
module AslBuild.Middleware.Types where

import           GHC.Generics

import           AslBuild.Types

data MiddlewareFlags
    = MiddlewareFlags
    { mwIp                :: String
    , mwPort              :: Int
    , mwNrThreads         :: Int
    , mwReplicationFactor :: Int
    , mwServers           :: [RemoteServerUrl]
    } deriving (Show, Eq, Generic)
