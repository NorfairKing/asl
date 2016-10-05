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
    , mwVerbosity         :: LogLevel
    } deriving (Show, Eq, Generic)

data LogLevel
    = LogOff
    | LogInfo
    | LogFine
    | LogFiner
    | LogFinest
    | LogAll
    deriving (Show, Eq, Generic, Enum)
