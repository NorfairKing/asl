{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Memcached.Types where

import GHC.Generics

import Data.Aeson

data MemcachedFlags = MemcachedFlags
    { memcachedPort :: Int
    , memcachedAsDaemon :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON MemcachedFlags

instance ToJSON MemcachedFlags
