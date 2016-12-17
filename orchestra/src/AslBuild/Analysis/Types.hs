{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AslBuild.Analysis.Types where

import           GHC.Generics

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Csv

data MetaAvgResults
    = MetaAvgResults
    { avgGetResults  :: Maybe MetaAvg
    , avgSetResults  :: Maybe MetaAvg
    , avgBothResults :: MetaAvg
    } deriving (Show, Eq, Generic)

instance FromJSON MetaAvgResults
instance ToJSON   MetaAvgResults

data MetaAvg
    = MetaAvg
    { avgAvgs    :: Double
    , stdDevAvgs :: Double
    , combStdDev :: Double
    } deriving (Show, Eq, Generic)

instance FromJSON MetaAvg
instance ToJSON   MetaAvg

instance Num MetaAvg where
    (MetaAvg aa1 sa1 ss1) + (MetaAvg aa2 sa2 ss2) = MetaAvg
        { avgAvgs = aa1 + aa2
        , stdDevAvgs = sqrt $ sa1 ** 2 + sa2 ** 2
        , combStdDev = sqrt $ ss1 ** 2 + ss2 ** 2
        }
    fromInteger i = MetaAvg (fromInteger i) 0 0

instance ToNamedRecord MetaAvg where
    toNamedRecord MetaAvg{..} = namedRecord
        [ "avgavg" .= avgAvgs
        , "avgstd" .= stdDevAvgs
        , "combstd" .= combStdDev
        ]

instance DefaultOrdered MetaAvg where
    headerOrder _ = header ["avgavg", "avgstd", "combstd"]

data AvgResults
    = AvgResults
    { getResults  :: Maybe Avg
    , setResults  :: Maybe Avg
    , bothResults :: Avg
    } deriving (Show, Eq, Generic)

instance FromJSON AvgResults
instance ToJSON   AvgResults

data Avg
    = Avg
    { avg    :: Double
    , stdDev :: Double
    } deriving (Show, Eq, Generic)

instance FromJSON Avg
instance ToJSON   Avg

instance ToNamedRecord Avg where
    toNamedRecord Avg{..} = namedRecord
        [ "avg" .= avg
        , "std" .= stdDev
        ]

instance DefaultOrdered Avg where
    headerOrder _ = header ["avg", "std"]

instance Num Avg where
    (Avg a1 s1) + (Avg a2 s2) = Avg (a1 + a2) (sqrt $ s1 ** 2 + s2 ** 2)
    fromInteger i = Avg (fromInteger i) 0

