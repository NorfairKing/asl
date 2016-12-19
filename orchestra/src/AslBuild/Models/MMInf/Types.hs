{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Models.MMInf.Types where

import GHC.Generics

import Data.Aeson

data MMInfModel = MMInfModel
    { arrivalRate :: Double -- λ: In messages / second
    , serviceRate :: Double -- μ: In messages / second
    } deriving (Show, Eq, Generic)

instance ToJSON MMInfModel

instance FromJSON MMInfModel
