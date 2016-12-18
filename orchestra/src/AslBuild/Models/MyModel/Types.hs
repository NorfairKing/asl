{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Models.MyModel.Types where

import           GHC.Generics

import           Data.Aeson

data MyModel
    = MyModel
    { overallArrivalRate  :: Double
    , acceptorServiceTime :: Double
    , getServiceTime      :: Double
    , getNrServers        :: Double
    , setServiceTime      :: Double
    , setIServiceTime     :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON MyModel
instance FromJSON MyModel
