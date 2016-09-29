{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Baseline.Types where

import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LB
import           Data.Csv
import           GHC.Generics

import           AslBuild.Memaslap
import           AslBuild.Types

data BaselineExperimentRuleCfg
    = BaselineExperimentRuleCfg
    { target        :: String
    , csvOutFile    :: FilePath
    , localLogfile  :: FilePath
    , baselineSetup :: BaseLineSetup
    }

data BaseLineSetup
    = BaseLineSetup
    { repetitions         :: Int
    , runtime             :: Int
    , maxNrVirtualClients :: Int
    }

data BaselineExperimentSetup
    = BaselineExperimentSetup
    { clientSetups :: [ClientSetup]
    , serverSetup  :: ServerSetup
    } deriving (Show, Eq, Generic)

instance ToJSON   BaselineExperimentSetup
instance FromJSON BaselineExperimentSetup

data ClientSetup
    = ClientSetup
    { cRemoteLogin             :: RemoteLogin
    , cLocalLog                :: FilePath
    , cRemoteLog               :: FilePath
    , cResultsFile             :: FilePath
    , cLocalMemaslapConfigFile :: FilePath
    , cMemaslapSettings        :: MemaslapSettings
    } deriving (Show, Eq, Generic)

instance ToJSON   ClientSetup
instance FromJSON ClientSetup

data ServerSetup
    = ServerSetup
    { sRemoteLogin :: RemoteLogin
    } deriving (Show, Eq, Generic)

instance ToJSON   ServerSetup
instance FromJSON ServerSetup

data ExperimentResults
    = ExperimentResults
    { erSetup       :: BaselineExperimentSetup
    , erClientSetup :: ClientSetup
    , erMemaslapLog :: MemaslapLog
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResults
instance FromJSON ExperimentResults

resultsCsv :: [ExperimentResults] -> LB.ByteString
resultsCsv = encodeByName $ header
    [ "nrClients"
    , "threads"
    , "concurrency"
    , "overwrite"
    , "time"
    , "avg"
    , "std"
    , "tps"
    ]

instance ToNamedRecord ExperimentResults where
    toNamedRecord ExperimentResults{..} =
        let BaselineExperimentSetup{..} = erSetup
            ClientSetup{..} = erClientSetup
            MemaslapLog{..} = erMemaslapLog
            MemaslapSettings{..} = cMemaslapSettings
            MemaslapFlags{..} = msFlags
        in namedRecord
            [ "nrClients" .= length clientSetups
            , "threads" .= msThreads
            , "concurrency" .= msConcurrency
            , "overwrite" .= msOverwrite
            , "time" .= msTime
            , "avg" .= avg
            , "std" .= std
            , "tps" .= tps
            ]
