{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AslBuild.Memaslap.Types where

import Data.Aeson
import GHC.Generics

import AslBuild.Types

data MemaslapSettings = MemaslapSettings
    { msConfig :: MemaslapConfig
    , msFlags :: MemaslapFlags
    } deriving (Show, Eq, Generic)

instance ToJSON MemaslapSettings

instance FromJSON MemaslapSettings

data MemaslapConfig = MemaslapConfig
    { keysizeDistributions :: [Distribution]
    , valueDistributions :: [Distribution]
    , setProportion :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON MemaslapConfig

instance FromJSON MemaslapConfig

data Distribution = Distribution
    { distrMin :: Int
    , distrMax :: Int
    , distrProp :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON Distribution

instance FromJSON Distribution

data MemaslapFlags = MemaslapFlags
    { msServers :: [RemoteServerUrl]
    , msThreads :: Int
    , msConcurrency :: Int
    , msOverwrite :: Double
    , msStatFreq :: Maybe TimeUnit
    , msWorkload :: MemaslapWorkload
    , msConfigFile :: FilePath
    , msWindowSize :: WindowSize
    } deriving (Show, Eq, Generic)

instance ToJSON MemaslapFlags

instance FromJSON MemaslapFlags

data MemaslapWorkload
    = WorkFor TimeUnit
    | NrRequests Int
    deriving (Show, Eq, Generic)

instance ToJSON MemaslapWorkload

instance FromJSON MemaslapWorkload

data WindowSize
    = Unit Int
    | Kilo Int
    deriving (Show, Eq, Generic)

instance ToJSON WindowSize

instance FromJSON WindowSize

renderWindowSize :: WindowSize -> String
renderWindowSize (Unit i) = show i
renderWindowSize (Kilo i) = show i ++ "k"

data MemaslapLog = MemaslapLog
    { config :: () -- Fill in later if needed
    , triples :: [StatsTriple]
    , totalStatsTrip :: Maybe TotalStatsTrip
    , finalStats :: FinalStats
    } deriving (Show, Eq, Generic)

instance ToJSON MemaslapLog

instance FromJSON MemaslapLog

data StatsTriple = StatsTriple
    { getStats :: Maybe StatisticsLog
    , setStats :: Maybe StatisticsLog
    , bothStats :: StatisticsLog
    } deriving (Show, Eq, Generic)

instance ToJSON StatsTriple

instance FromJSON StatsTriple

data StatisticsLog = StatisticsLog
    { periodStats :: Statistics
    , globalStats :: Statistics
    } deriving (Show, Eq, Generic)

instance ToJSON StatisticsLog

instance FromJSON StatisticsLog

data Statistics = Statistics
    { time :: Int
    , ops :: Int
    , tps :: Int
    , net :: Double
    , getMiss :: Int
    , minUs :: Int
    , maxUs :: Int
    , avgUs :: Int
    , std :: Double
    , geoDist :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON Statistics where
    toJSON Statistics {..} =
        object
            [ "time" .= time
            , "ops" .= ops
            , "tps" .= tps
            , "net" .= net
            , "getMiss" .= getMiss
            , "minUs" .= minUs
            , "maxUs" .= maxUs
            , "avgUs" .= avgUs
            , "stdDev" .= std
            , "geoDist" .= geoDist
            ]

instance FromJSON Statistics where
    parseJSON (Object o) =
        Statistics <$> o .: "time" <*> o .: "ops" <*> o .: "tps" <*> o .: "net" <*> o .: "getMiss" <*>
        o .: "minUs" <*>
        o .: "maxUs" <*>
        o .: "avgUs" <*>
        o .: "stdDev" <*>
        o .: "geoDist"
    parseJSON _ = mempty

data TotalStatsTrip = TotalStatsTrip
    { totalGetStats :: Maybe TotalStats
    , totalSetStats :: Maybe TotalStats
    , totalBothStats :: TotalStats
    } deriving (Show, Eq, Generic)

instance ToJSON TotalStatsTrip

instance FromJSON TotalStatsTrip

data TotalStats = TotalStats
    { totalEvents :: Int
    , totalMin :: Int
    , totalMax :: Int
    , totalAvg :: Int
    , totalGeo :: Double
    , totalStd :: Double
    , totalLog2Dist :: () -- Fill in later if necessary
    } deriving (Show, Eq, Generic)

instance ToJSON TotalStats

instance FromJSON TotalStats

data FinalStats = FinalStats
    { finalGets :: Int
    , finalSets :: Int
    , finalGetMisses :: Int
    , finalWrittenBytes :: Int
    , finalReadBytes :: Int
    , finalObjectBytes :: Int
    , finalRuntime :: Double
    , finalOps :: Int
    , finalTps :: Int
    , finalNetRate :: ()
    } deriving (Show, Eq, Generic)

instance ToJSON FinalStats

instance FromJSON FinalStats
