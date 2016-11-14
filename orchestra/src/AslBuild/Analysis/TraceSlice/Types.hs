{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Types where

import           GHC.Generics

import qualified Pipes.Csv      as P

import           Data.Csv

import           AslBuild.Types

data Durations
    = Durations
    { reqKind            :: RequestKind
    , arrivalTime        :: Integer
    , untilParsedTime    :: Integer
    , untilEnqueuedTime  :: Integer
    , untilDequeuedTime  :: Integer
    , untilAskedTime     :: Integer
    , untilRepliedTime   :: Integer
    , untilRespondedTime :: Integer
    } deriving (Show, Eq, Generic)

instance FromNamedRecord Durations where
    parseNamedRecord m = Durations
        <$> m .: "reqKind"
        <*> m .: "arrivalTime"
        <*> m .: "untilParsed"
        <*> m .: "untilEnqueued"
        <*> m .: "untilDequeued"
        <*> m .: "untilAsked"
        <*> m .: "untilReplied"
        <*> m .: "untilResponded"

instance ToNamedRecord Durations where
    toNamedRecord Durations{..} =
        namedRecord
            [ "reqKind" .= reqKind
            , "arrivalTime" .= arrivalTime
            , "untilParsed" .= untilParsedTime
            , "untilEnqueued" .= untilEnqueuedTime
            , "untilDequeued" .= untilDequeuedTime
            , "untilAsked" .= untilAskedTime
            , "untilReplied" .= untilRepliedTime
            , "untilResponded" .= untilRespondedTime
            ]

instance DefaultOrdered Durations where
    headerOrder _ = header
        [ "reqKind"
        , "arrivalTime"
        , "untilParsed"
        , "untilEnqueued"
        , "untilDequeued"
        , "untilAsked"
        , "untilReplied"
        , "untilResponded"
        ]

data DurTup
    = DurTup
    { nrCs             :: Int
    , tilParsedTime    :: Integer
    , tilEnqueuedTime  :: Integer
    , tilDequeuedTime  :: Integer
    , tilAskedTime     :: Integer
    , tilRepliedTime   :: Integer
    , tilRespondedTime :: Integer
    }

instance FromNamedRecord DurTup where
    parseNamedRecord m = DurTup
        <$> m .: "nrClients"
        <*> m .: "untilParsed"
        <*> m .: "untilEnqueued"
        <*> m .: "untilDequeued"
        <*> m .: "untilAsked"
        <*> m .: "untilReplied"
        <*> m .: "untilResponded"

instance ToNamedRecord DurTup where
    toNamedRecord DurTup{..} =
        namedRecord
            [ "nrClients" .= nrCs
            , "untilParsed" .= tilParsedTime
            , "untilEnqueued" .= tilEnqueuedTime
            , "untilDequeued" .= tilDequeuedTime
            , "untilAsked" .= tilAskedTime
            , "untilReplied" .= tilRepliedTime
            , "untilResponded" .= tilRespondedTime
            ]

instance DefaultOrdered DurTup where
    headerOrder _ = header
        [ "nrClients"
        , "untilParsed"
        , "untilEnqueued"
        , "untilDequeued"
        , "untilAsked"
        , "untilReplied"
        , "untilResponded"
        ]

data DurationsLine a
    = DurationsLine
    { nrCls    :: Int
    , category :: String
    , value    :: a
    } deriving (Show, Eq, Generic)

instance ToField a => ToNamedRecord (DurationsLine a) where
    toNamedRecord DurationsLine{..} =
        namedRecord
            [ "nrClients" .= nrCls
            , "category" .= category
            , "value" .= value
            ]

instance DefaultOrdered (DurationsLine a) where
    headerOrder _ = header
        [ "nrClients"
        , "category"
        , "value"
        ]

