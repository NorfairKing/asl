{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Types where

import           GHC.Generics

import qualified Pipes.Csv                     as P

import           Data.Csv
import           Data.Monoid

import           AslBuild.Analysis.Trace.Types

data DurTup a
    = DurTup
    { nrCs      :: Int
    , middleTds :: Int
    , durs      :: Durations a
    } deriving (Show, Eq, Generic)

instance Functor DurTup where
    fmap f dt = dt { durs = fmap f (durs dt) }

instance FromField a => FromNamedRecord (DurTup a) where
    parseNamedRecord m = DurTup
        <$> m .: "nrClients"
        <*> m .: "middleThreads"
        <*> parseNamedRecord m

instance ToField a => ToNamedRecord (DurTup a) where
    toNamedRecord DurTup{..} =
        namedRecord
            [ "nrClients" .= nrCs
            , "middleThreads" .= middleTds
            ]
        <> toNamedRecord durs

instance DefaultOrdered (DurTup a) where
    headerOrder _ = header
        [ "nrClients"
        , "middleThreads"
        ]
        <> headerOrder (undefined :: Durations Integer)

data DurationsLine a
    = DurationsLine
    { nrCls      :: Int
    , middleThds :: Int
    , category   :: String
    , value      :: a
    } deriving (Show, Eq, Generic)

instance ToField a => ToNamedRecord (DurationsLine a) where
    toNamedRecord DurationsLine{..} =
        namedRecord
            [ "nrClients" .= nrCls
            , "middleThreads" .= middleThds
            , "category" .= category
            , "value" .= value
            ]

instance DefaultOrdered (DurationsLine a) where
    headerOrder _ = header
        [ "nrClients"
        , "middleThreads"
        , "category"
        , "value"
        ]

