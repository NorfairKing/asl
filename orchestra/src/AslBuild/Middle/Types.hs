{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Middle.Types where

import GHC.Generics

import Data.Aeson

import AslBuild.Middleware.Types
import AslBuild.Types

data MiddleSetup = MiddleSetup
    { mRemoteLogin :: RemoteLogin
    , mLocalTrace :: FilePath
    , mMiddlewareFlags :: MiddlewareFlags
    } deriving (Show, Eq, Generic)

instance FromJSON MiddleSetup

instance ToJSON MiddleSetup
