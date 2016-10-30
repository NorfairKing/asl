{-# LANGUAGE DeriveGeneric #-}
module AslBuild.Experiment.Types where

import           Data.Aeson
import           GHC.Generics

import           AslBuild.Client.Types
import           AslBuild.Memaslap.Types
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Server.Types
import           AslBuild.Types

data ExperimentCfg
    = ExperimentCfg
    { target    :: String
    , nrClients :: Int
    , nrServers :: Int
    , location  :: Location
    , runtime   :: TimeUnit
    , logLevel  :: LogLevel
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentCfg
instance FromJSON ExperimentCfg

data Location
    = Local
    | Remote
    deriving (Show, Eq, Generic)

instance ToJSON   Location
instance FromJSON Location

data ExperimentSetup
    = ExperimentSetup
    { esRuntime    :: TimeUnit
    , clientSetups :: [ClientSetup]
    , middleSetup  :: MiddleSetup
    , serverSetups :: [ServerSetup]
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentSetup
instance FromJSON ExperimentSetup

data ExperimentResultSummary
    = ExperimentResultSummary
    { erClientResults :: [FilePath]
    , erMiddleResults :: FilePath
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResultSummary
instance FromJSON ExperimentResultSummary

data ClientResults
    = ClientResults
    { crSetup :: ClientSetup
    , crLog   :: MemaslapLog
    } deriving (Show, Eq, Generic)

instance ToJSON   ClientResults
instance FromJSON ClientResults


data ExperimentResults
    = ExperimentResults
    { cResults :: [ClientResults]
    , mResults :: [MiddleResultLine]
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResults
instance FromJSON ExperimentResults

data MiddleResultLine
    = MiddleResultLine
    { requestSuccess       :: Bool
    , requestReceivedTime  :: Integer
    , requestParsedTime    :: Integer
    , requestEnqueuedTime  :: Integer
    , requestDequeuedTime  :: Integer
    , requestAskedTime     :: Integer
    , requestRepliedTime   :: Integer
    , requestRespondedTime :: Integer
    } deriving (Show, Eq, Generic)

instance ToJSON   MiddleResultLine
instance FromJSON MiddleResultLine
