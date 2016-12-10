{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Models.MMm.Types where

import           GHC.Generics

import           Data.Aeson

import           AslBuild.Analysis.Types


data MMmModel
    = MMmModel
    { arrivalRate :: Avg -- λ: In messages / second
    , serviceRate :: Avg -- μ: In messages / second
    , nrServers   :: Int -- m: in #
    } deriving (Show, Eq, Generic)

instance FromJSON MMmModel
instance ToJSON   MMmModel
