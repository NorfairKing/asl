{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Types where

import           GHC.Generics

import qualified Pipes.Csv      as P

import           Data.Csv

import           AslBuild.Types

data TraceSliceAnalysisCfg
    = TraceSliceAnalysisCfg
    { analysisTarget       :: String
    , summaryLocationsPath :: FilePath
    , analysisOutDir       :: FilePath
    } deriving (Show, Eq)

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

data DurationsLine a
    = DurationsLine
    { rKind    :: RequestKind
    , aTime    :: Integer
    , category :: String
    , value    :: a
    } deriving (Show, Eq, Generic)

instance ToField a => ToNamedRecord (DurationsLine a) where
    toNamedRecord DurationsLine{..} =
        namedRecord
            [ "rKind" .= rKind
            , "aTime" .= aTime
            , "category" .= category
            , "value" .= value
            ]

instance DefaultOrdered (DurationsLine a) where
    headerOrder _ = header
        [ "rKind"
        , "aTime"
        , "category"
        , "value"
        ]

