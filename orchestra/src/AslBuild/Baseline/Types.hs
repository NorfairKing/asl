{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Baseline.Types where

import           Data.Aeson               (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy     as LB
import           Data.Csv
import           GHC.Generics

import           AslBuild.Memaslap
import           AslBuild.Memcached.Types
import           AslBuild.Types


data BaselineExperimentRuleCfg
    = BaselineExperimentRuleCfg
    { target                       :: String
    , csvOutFile                   :: FilePath
    , localLogfile                 :: FilePath
    , maxNrClients                 :: Int
    , baselineExperimentsCacheFile :: FilePath
    , baselineLocation             :: BaselineLocation
    , baselineSetup                :: BaseLineSetup
    } deriving (Show, Eq, Generic)

data BaselineLocation
    = BaselineLocal
    | BaselineRemote
    deriving (Show, Eq, Generic)

data BaseLineSetup
    = BaseLineSetup
    { repetitions         :: Int
    , runtime             :: Int
    , maxNrVirtualClients :: Int
    } deriving (Show, Eq, Generic)

data BaselineExperimentSetup
    = BaselineExperimentSetup
    { repetition   :: Int
    , clientSetups :: [ClientSetup]
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
    , cIndex                   :: Int
    , cMemaslapSettings        :: MemaslapSettings
    } deriving (Show, Eq, Generic)

instance ToJSON   ClientSetup
instance FromJSON ClientSetup

data ServerSetup
    = ServerSetup
    { sRemoteLogin    :: RemoteLogin
    , sMemcachedFlags :: MemcachedFlags
    } deriving (Show, Eq, Generic)

instance ToJSON   ServerSetup
instance FromJSON ServerSetup

data ExperimentResults
    = ExperimentResults
    { erSetup       :: BaselineExperimentSetup
    , erClientSetup :: ClientSetup
    , erMemaslapLog :: MemaslapLog
    , erClientIndex :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResults
instance FromJSON ExperimentResults

resultsCsv :: [ExperimentResults] -> LB.ByteString
resultsCsv = encodeByName $ header
    [ "nrClients"
    , "clientIndex"
    , "rep"
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
            , "clientIndex" .= erClientIndex
            , "rep" .= repetition
            , "threads" .= msThreads
            , "concurrency" .= msConcurrency
            , "overwrite" .= msOverwrite
            , "time" .= msTimeUnsafe msWorkload
            , "avg" .= avg
            , "std" .= std
            , "tps" .= tps
            ]
