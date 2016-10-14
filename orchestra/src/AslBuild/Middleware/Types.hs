{-# LANGUAGE DeriveGeneric #-}
module AslBuild.Middleware.Types where

import           Data.Aeson
import           GHC.Generics

import           AslBuild.Types

data MiddlewareFlags
    = MiddlewareFlags
    { mwIp                :: String
    , mwPort              :: Int
    , mwNrThreads         :: Int
    , mwReplicationFactor :: Int
    , mwServers           :: [RemoteServerUrl]
    , mwVerbosity         :: LogLevel
    , mwTraceFile         :: FilePath
    } deriving (Show, Eq, Generic)

instance FromJSON MiddlewareFlags
instance ToJSON   MiddlewareFlags

data LogLevel
    = LogOff
    | LogInfo
    | LogFine
    | LogFiner
    | LogFinest
    | LogAll
    deriving (Show, Eq, Generic, Enum)

instance FromJSON LogLevel
instance ToJSON   LogLevel
