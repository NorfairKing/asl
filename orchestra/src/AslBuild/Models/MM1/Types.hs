{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Models.MM1.Types where

import GHC.Generics

import Data.Aeson

data MM1Model = MM1Model
    { arrivalRate :: Double -- λ: In messages / second
    , serviceRate :: Double -- μ: In messages / second
    } deriving (Show, Eq, Generic)

instance FromJSON MM1Model

instance ToJSON MM1Model
