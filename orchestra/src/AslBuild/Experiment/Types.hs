{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Experiment.Types where

import           Data.Aeson
import           GHC.Generics

import           Development.Shake

import           AslBuild.Client.Types
import           AslBuild.Memaslap.Types
import           AslBuild.Middle.Types
import           AslBuild.Server.Types
import           AslBuild.Types
import           AslBuild.Vm.Types

class ExperimentConfig a where
    highLevelConfig :: a -> HighLevelConfig
    genExperimentSetups :: a -> Action ([ExperimentSetup], [VmData])

experimentTarget :: ExperimentConfig a => a -> String
experimentTarget = target . highLevelConfig

data HighLevelConfig
    = HighLevelConfig
    { target    :: String
    , nrClients :: Int
    , nrServers :: Int
    , location  :: Location
    } deriving (Show, Eq, Generic)

instance ToJSON   HighLevelConfig
instance FromJSON HighLevelConfig

data ExperimentSetup
    = ExperimentSetup
    { esRuntime            :: TimeUnit
    , esResultsSummaryFile :: FilePath
    , esSetupFile          :: FilePath
    , clientSetups         :: [ClientSetup]
    , middleSetup          :: MiddleSetup
    , serverSetups         :: [ServerSetup]
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentSetup
instance FromJSON ExperimentSetup

data ExperimentResultSummary
    = ExperimentResultSummary
    { erClientResultsFiles :: [FilePath]
    , erMiddleResultsFile  :: FilePath
    , erSetupFile          :: FilePath
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResultSummary where
    toJSON ExperimentResultSummary{..} = object
        [ "client-results-files" .= erClientResultsFiles
        , "middle-results-file" .= erMiddleResultsFile
        , "setup" .= erSetupFile
        ]

instance FromJSON ExperimentResultSummary where
    parseJSON (Object o) = ExperimentResultSummary
        <$> o .: "client-results-files"
        <*> o .: "middle-results-file"
        <*> o .: "setup"
    parseJSON _ = mempty

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
