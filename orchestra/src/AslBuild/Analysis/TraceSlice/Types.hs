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

durationsHeader :: Header
durationsHeader = header
    [ "reqKind"
    , "arrivalTime"
    , "untilParsed"
    , "untilEnqueued"
    , "untilDequeued"
    , "untilAsked"
    , "untilReplied"
    , "untilResponded"
    ]

data DurationsLine
    = DurationsLine
    { rKind    :: RequestKind
    , aTime    :: Integer
    , category :: String
    , value    :: Integer
    } deriving (Show, Eq, Generic)

instance ToNamedRecord DurationsLine where
    toNamedRecord DurationsLine{..} =
        namedRecord
            [ "rKind" .= rKind
            , "aTime" .= aTime
            , "category" .= category
            , "value" .= value
            ]

durationsLineHeader :: Header
durationsLineHeader = header
    [ "rKind"
    , "aTime"
    , "category"
    , "value"
    ]

