{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Memaslap.Types where

import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.Csv
import           Data.List
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
    , msStatFreq    :: TimeUnit
    , msTime        :: TimeUnit
    , msConfigFile  :: FilePath
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapFlags
instance FromJSON MemaslapFlags

data TimeUnit
    = Seconds Int
    deriving (Show, Eq, Generic)

instance ToJSON   TimeUnit
instance FromJSON TimeUnit

instance ToField TimeUnit where
    toField = toField . toSeconds

toSeconds :: TimeUnit -> Int
toSeconds (Seconds i) = i

timeUnit :: TimeUnit -> String
timeUnit (Seconds i) = show i ++ "s"

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
