{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Memaslap.Types where

import           Data.Aeson     (FromJSON, ToJSON)
import           Data.Csv
import           GHC.Generics

import           AslBuild.Types

data MemaslapSettings
    = MemaslapSettings
    { msConfig :: MemaslapConfig
    , msFlags  :: MemaslapFlags
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapSettings
instance FromJSON MemaslapSettings

data MemaslapConfig
    = MemaslapConfig
    { keysizeDistributions :: [Distribution]
    , valueDistributions   :: [Distribution]
    , setProportion        :: Double
    , getProportion        :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapConfig
instance FromJSON MemaslapConfig

data Distribution
    = Distribution
    { distrMin  :: Int
    , distrMax  :: Int
    , distrProp :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON   Distribution
instance FromJSON Distribution

data MemaslapFlags
    = MemaslapFlags
    { msServers     :: [RemoteServerUrl]
    , msThreads     :: Int
    , msConcurrency :: Int
    , msOverwrite   :: Double
    , msStatFreq    :: Maybe TimeUnit
    , msWorkload    :: MemaslapWorkload
    , msConfigFile  :: FilePath
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapFlags
instance FromJSON MemaslapFlags

data MemaslapWorkload
    = WorkFor TimeUnit
    | NrRequests Int
    deriving (Show, Eq, Generic)

instance ToJSON   MemaslapWorkload
instance FromJSON MemaslapWorkload

data MemaslapLog
    = MemaslapLog
    { avg :: Double
    , std :: Double
    , tps :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapLog
instance FromJSON MemaslapLog

instance ToNamedRecord MemaslapLog where
    toNamedRecord MemaslapLog{..} = namedRecord
        [ "avg" .= avg, "std" .= std, "tps" .= tps]
