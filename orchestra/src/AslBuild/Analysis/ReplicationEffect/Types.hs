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
    { nrServers         :: Int
    , replicationFactor :: Int
    , kind              :: RequestKind
    , respAvg           :: Avg
    } deriving (Show, Eq, Generic)

instance ToNamedRecord SimplifiedCsvLine where
    toNamedRecord SimplifiedCsvLine{..} = namedRecord
        [ "nrServers" .= nrServers
        , "replicationFactor" .= replicationFactor
        , "kind" .= kind
        ] <> toNamedRecord respAvg

instance DefaultOrdered SimplifiedCsvLine where
    headerOrder _ = header
        [ "nrServers"
        , "replicationFactor"
        , "kind"
        ] <> headerOrder (undefined :: Avg)


