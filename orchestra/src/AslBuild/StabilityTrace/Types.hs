{-# LANGUAGE DeriveGeneric #-}
module AslBuild.StabilityTrace.Types where

import           Data.Aeson
import           GHC.Generics

import           AslBuild.Client.Types
import           AslBuild.Memaslap.Types
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Server.Types
import           AslBuild.Types

data StabilityTraceCfg
    = StabilityTraceCfg
    { target     :: String
    , csvOutFile :: FilePath
    , nrClients  :: Int
    , nrServers  :: Int
    , location   :: StabilityLocation
    , runtime    :: TimeUnit
    , logLevel   :: LogLevel
    } deriving (Show, Eq, Generic)

instance ToJSON   StabilityTraceCfg
instance FromJSON StabilityTraceCfg

data StabilityLocation
    = StabilityLocal
    | StabilityRemote
    deriving (Show, Eq, Generic)

instance ToJSON   StabilityLocation
instance FromJSON StabilityLocation

data StabilityTraceSetup
    = StabilityTraceSetup
    { stsRuntime   :: TimeUnit
    , clientSetups :: [ClientSetup]
    , middleSetup  :: MiddleSetup
    , serverSetups :: [ServerSetup]
    } deriving (Show, Eq, Generic)

instance ToJSON   StabilityTraceSetup
instance FromJSON StabilityTraceSetup

data StabilityTraceExperimentResults
    = StabilityTraceExperimentResults
    { sterClientIndex :: Int
    , sterClientSetup :: ClientSetup
    , sterMemaslapLog :: MemaslapLog
    } deriving (Show, Eq, Generic)

instance ToJSON   StabilityTraceExperimentResults
instance FromJSON StabilityTraceExperimentResults

