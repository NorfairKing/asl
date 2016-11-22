{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.WriteEffect.Types where

import           Data.Monoid
import           GHC.Generics

import           Data.Csv
import qualified Data.Vector             as V

import           AslBuild.Analysis.Types
import           AslBuild.Utils

data SimplifiedCsvLine
    = SimplifiedCsvLine
    { nrServers         :: Int
    , writePercentage   :: Double
    , replicationFactor :: Int
    , respAvg           :: Avg
    , tpsAvg            :: Avg
    } deriving (Show, Eq, Generic)

instance ToNamedRecord SimplifiedCsvLine where
    toNamedRecord SimplifiedCsvLine{..} = namedRecord
        [ "nrServers" .= nrServers
        , "writePercentage" .= writePercentage
        , "replicationFactor" .= replicationFactor
        ] <> mapKeys ("resp" <>) (toNamedRecord respAvg)
          <> mapKeys ("tps" <>)   (toNamedRecord tpsAvg)

instance DefaultOrdered SimplifiedCsvLine where
    headerOrder _ = header
        [ "nrServers"
        , "writePercentage"
        , "replicationFactor"
        ] <> V.map ("resp" <>) (headerOrder (undefined :: Avg))
          <> V.map ("tps" <>)  (headerOrder (undefined :: Avg))
