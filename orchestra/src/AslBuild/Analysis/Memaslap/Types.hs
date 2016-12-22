{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AslBuild.Analysis.Memaslap.Types where

import GHC.Generics

import Data.Aeson

import AslBuild.Analysis.Types

data CombinedClientResults = CombinedClientResults
    { avgTpsResults :: MetaAvgResults
    , avgRespResults :: MetaAvgResults
    , avgMinTps :: Avg
    , avgMaxTps :: Avg
    , avgMinResp :: Avg
    , avgMaxResp :: Avg
    } deriving (Show, Eq, Generic)

instance FromJSON CombinedClientResults

instance ToJSON CombinedClientResults

data MemaslapClientResults = MemaslapClientResults
    { tpsResults :: AvgResults
    , respResults :: AvgResults
    , minTps :: Double
    , maxTps :: Double
    , minResp :: Double
    , maxResp :: Double
    } deriving (Show, Eq, Generic)

instance FromJSON MemaslapClientResults

instance ToJSON MemaslapClientResults
