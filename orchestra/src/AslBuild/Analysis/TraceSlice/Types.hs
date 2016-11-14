{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Types where

import           GHC.Generics

import qualified Pipes.Csv      as P

import           Data.Csv

import           AslBuild.Types

class Monoid a => Mean a where
    combines :: [a] -> a
    divide :: Integral i => a -> i -> a
    uncombine :: a -> a -> a

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


instance Monoid Durations where
    mempty = Durations
        { reqKind = READ -- Fixme
        , arrivalTime        = 0
        , untilParsedTime    = 0
        , untilEnqueuedTime  = 0
        , untilDequeuedTime  = 0
        , untilAskedTime     = 0
        , untilRepliedTime   = 0
        , untilRespondedTime = 0
        }
    mappend d1 d2 =
        let s func = func d1 + func d2
        in Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = arrivalTime d2
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }

instance Mean Durations where
    combines ds =
        let s func = sum $ map func ds
        in Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = arrivalTime $ last ds
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }
    divide d i =
        let s func = func d `div` fromIntegral i
        in Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = arrivalTime d
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }
    uncombine d1 d2 =
        let s func = func d1 - func d2
        in Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = arrivalTime d1
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }


data DurTup
    = DurTup
    { nrCs             :: Int
    , middleTds        :: Int
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
        <*> m .: "middleThreads"
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
            , "middleThreads" .= middleTds
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
        , "middleThreads"
        , "untilParsed"
        , "untilEnqueued"
        , "untilDequeued"
        , "untilAsked"
        , "untilReplied"
        , "untilResponded"
        ]

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

