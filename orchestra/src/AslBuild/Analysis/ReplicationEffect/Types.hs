{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.ReplicationEffect.Types where

import           Data.Monoid
import           GHC.Generics

import           Data.Csv

import           AslBuild.Analysis.Types
import           AslBuild.Types

data SimplifiedCsvLine
    = SimplifiedCsvLine
    { nrServers              :: Int
    , replicationFactor      :: Int
    , replicationCoefficient :: Double
    , kind                   :: RequestKind
    , respAvg                :: Avg
    } deriving (Show, Eq, Generic)

instance ToNamedRecord SimplifiedCsvLine where
    toNamedRecord SimplifiedCsvLine{..} = namedRecord
        [ "nrServers" .= nrServers
        , "replicationFactor" .= replicationFactor
        , "replicationCoefficient" .= replicationCoefficient
        , "kind" .= kind
        ] <> toNamedRecord respAvg

instance DefaultOrdered SimplifiedCsvLine where
    headerOrder _ = header
        [ "nrServers"
        , "replicationFactor"
        , "replicationCoefficient"
        , "kind"
        ] <> headerOrder (undefined :: Avg)


data SimplifiedCostCsvLine
    = SimplifiedCostCsvLine
    { nrSs     :: Int
    , rf       :: Int
    , knd      :: RequestKind
    , time     :: Integer
    , category :: String
    } deriving (Show, Eq, Generic)

instance ToNamedRecord SimplifiedCostCsvLine where
    toNamedRecord SimplifiedCostCsvLine{..} = namedRecord
        [ "nrServers" .= nrSs
        , "replicationFactor" .= rf
        , "kind" .= knd
        , "time" .= time
        , "category" .= category
        ]

instance DefaultOrdered SimplifiedCostCsvLine where
    headerOrder _ = header
        [ "nrServers"
        , "replicationFactor"
        , "kind"
        , "time"
        , "category"
        ]
