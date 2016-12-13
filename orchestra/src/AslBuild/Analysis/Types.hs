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
        [ "avg" .= (floor avg :: Integer)
        , "std" .= stdDev
        ]

instance DefaultOrdered Avg where
    headerOrder _ = header ["avg", "std"]


