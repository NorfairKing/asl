{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Models.MMm.Types where

import           GHC.Generics

import           Data.Aeson

data MMmModel
    = MMmModel
    { arrivalRate :: Double -- λ: In messages / second
    , serviceRate :: Double -- μ: In messages / second
    , nrServers   :: Int -- m: in #
    } deriving (Show, Eq, Generic)

instance FromJSON MMmModel
instance ToJSON   MMmModel
