{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Models.MM1.Types where

import           GHC.Generics

import           Data.Aeson

import           AslBuild.Analysis.Types


data MM1Model
    = MM1Model
    { arrivalRate :: Avg -- λ: In messages / second
    , serviceRate :: Avg -- μ: In messages / second
    } deriving (Show, Eq, Generic)

instance FromJSON MM1Model
instance ToJSON   MM1Model
