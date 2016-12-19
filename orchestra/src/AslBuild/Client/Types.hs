{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AslBuild.Client.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import AslBuild.Memaslap
import AslBuild.Types

data ClientSetup = ClientSetup
    { cRemoteLogin :: RemoteLogin
    , cIndex :: Int
    , cLocalLog :: FilePath
    , cRemoteLog :: FilePath
    , cLocalMemaslapConfigFile :: FilePath
    , cMemaslapSettings :: MemaslapSettings
    } deriving (Show, Eq, Generic)

instance ToJSON ClientSetup

instance FromJSON ClientSetup
