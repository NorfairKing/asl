{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module AslBuild.Analysis.Memaslap.Types where

import           GHC.Generics

import           AslBuild.Analysis.Types


data MemaslapClientResults
    = MemaslapClientResults
    { tpsResults  :: AvgResults
    , respResults :: AvgResults
    } deriving (Show, Eq, Generic)
