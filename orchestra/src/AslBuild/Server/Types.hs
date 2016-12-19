{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AslBuild.Server.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import AslBuild.Memcached.Types
import AslBuild.Types

data ServerSetup = ServerSetup
    { sRemoteLogin :: RemoteLogin
    , sIndex :: Int
    , sMemcachedFlags :: MemcachedFlags
    } deriving (Show, Eq, Generic)

instance ToJSON ServerSetup

instance FromJSON ServerSetup
