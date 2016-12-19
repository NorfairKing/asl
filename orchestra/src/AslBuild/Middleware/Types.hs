{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Middleware.Types where

import Data.Aeson
import GHC.Generics

import AslBuild.Types

data MiddlewareFlags = MiddlewareFlags
    { mwIp :: String
    , mwPort :: Int
    , mwNrThreads :: Int
    , mwReplicationFactor :: Int
    , mwServers :: [RemoteServerUrl]
    , mwVerbosity :: LogLevel
    , mwTraceFile :: FilePath
    , mwReadSampleRate :: Maybe Int
    , mwWriteSampleRate :: Maybe Int
    } deriving (Show, Eq, Generic)

instance FromJSON MiddlewareFlags

instance ToJSON MiddlewareFlags

data LogLevel
    = LogOff
    | LogInfo
    | LogFine
    | LogFiner
    | LogFinest
    | LogAll
    deriving (Show, Eq, Generic)

logLevelInt :: LogLevel -> Int
logLevelInt LogOff = 0
logLevelInt LogInfo = 1
logLevelInt LogFine = 2
logLevelInt LogFiner = 3
logLevelInt LogFinest = 4
logLevelInt LogAll = 5

instance FromJSON LogLevel

instance ToJSON LogLevel
