{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.WriteEffect.Types where

import           Control.Arrow
import           Data.Monoid
import           GHC.Generics

import           Data.Csv
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import qualified Data.Vector             as V

import           AslBuild.Analysis.Types
import           AslBuild.Types

data SimplifiedCsvLine
    = SimplifiedCsvLine
    { nrServers         :: Int
    , writePercentage   :: Double
    , replicationFactor :: Int
    , respAvg           :: Avg
    , tpsAvg            ::Avg
    } deriving (Show, Eq, Generic)

instance ToNamedRecord SimplifiedCsvLine where
    toNamedRecord SimplifiedCsvLine{..} = namedRecord
        [ "nrServers" .= nrServers
        , "writePercentage" .= writePercentage
        , "replicationFactor" .= replicationFactor
        ] <> mapKeys ("resp" <>) (toNamedRecord respAvg)
          <> mapKeys ("tps" <>)   (toNamedRecord tpsAvg)

mapKeys :: (Eq l, Hashable l) => (k -> l) -> HashMap k v -> HashMap l v
mapKeys func hm = HM.fromList $ map (first func) $ HM.toList hm

instance DefaultOrdered SimplifiedCsvLine where
    headerOrder _ = header
        [ "nrServers"
        , "writePercentage"
        , "replicationFactor"
        ] <> V.map ("resp" <>) (headerOrder (undefined :: Avg))
          <> V.map ("tps" <>)  (headerOrder (undefined :: Avg))
