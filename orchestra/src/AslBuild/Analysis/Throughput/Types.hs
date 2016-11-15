{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.Throughput.Types where

import           GHC.Generics

import           Data.Csv

data ThroughputResults
    = ThroughputResults
    { getThroughputResults  :: Maybe Avg
    , setThroughputResults  :: Maybe Avg
    , bothThroughputResults :: Avg
    } deriving (Show, Eq, Generic)

data Avg
    = Avg
    { avg    :: Double
    , stdDev :: Double
    } deriving (Show, Eq, Generic)

instance ToNamedRecord Avg where
    toNamedRecord Avg{..} = namedRecord
        [ "avg" .= (floor avg :: Integer)
        , "std" .= stdDev
        ]

instance DefaultOrdered Avg where
    headerOrder _ = header ["avg", "std"]


