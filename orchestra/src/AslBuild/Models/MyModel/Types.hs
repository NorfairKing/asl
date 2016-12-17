{-# LANGUAGE DeriveGeneric #-}

module AslBuild.Models.MyModel.Types where

import           GHC.Generics

import           Data.Aeson

import           AslBuild.Models.MM1.Types
import           AslBuild.Models.MMInf.Types
import           AslBuild.Models.MMm.Types

data MyModel
    = MyModel
    { acceptorModel :: MM1Model
    , getModel      :: MMmModel
    , setModel      :: MM1Model
    , setModelInf   :: MMInfModel
    } deriving (Show, Eq, Generic)

instance ToJSON MyModel
instance FromJSON MyModel
