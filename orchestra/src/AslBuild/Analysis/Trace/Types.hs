{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.Trace.Types where

import           Data.Aeson     (FromJSON, ToJSON)
import           Data.Monoid
import           GHC.Generics

import           Data.Csv

import           AslBuild.Types

class Monoid a => Mean a where
    combines :: [a] -> a
    divide :: Integral i => a -> i -> a
    uncombine :: a -> a -> a

data MiddleDurationsLine
    = MiddleDurationsLine
    { reqKind     :: RequestKind
    , arrivalTime :: Integer
    , durations   :: Durations Integer
    } deriving (Show, Eq, Generic)

instance FromNamedRecord MiddleDurationsLine where
    parseNamedRecord m = MiddleDurationsLine
        <$> m .: "reqKind"
        <*> m .: "arrivalTime"
        <*> parseNamedRecord m

instance ToNamedRecord MiddleDurationsLine where
    toNamedRecord MiddleDurationsLine{..} =
        namedRecord
            [ "reqKind" .= reqKind
            , "arrivalTime" .= arrivalTime
            ]
        <> toNamedRecord durations

instance DefaultOrdered MiddleDurationsLine where
    headerOrder _ = header
        [ "reqKind"
        , "arrivalTime"
        ] <> headerOrder (undefined :: Durations Integer)


data Durations a
    = Durations
    { untilParsedTime    :: a
    , untilEnqueuedTime  :: a
    , untilDequeuedTime  :: a
    , untilAskedTime     :: a
    , untilRepliedTime   :: a
    , untilRespondedTime :: a
    } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (Durations a)
instance FromJSON a => FromJSON (Durations a)

instance FromField a => FromNamedRecord (Durations a) where
    parseNamedRecord m = Durations
        <$> m .: "untilParsed"
        <*> m .: "untilEnqueued"
        <*> m .: "untilDequeued"
        <*> m .: "untilAsked"
        <*> m .: "untilReplied"
        <*> m .: "untilResponded"

instance ToField a => ToNamedRecord (Durations a) where
    toNamedRecord Durations{..} =
        namedRecord
            [ "untilParsed" .= untilParsedTime
            , "untilEnqueued" .= untilEnqueuedTime
            , "untilDequeued" .= untilDequeuedTime
            , "untilAsked" .= untilAskedTime
            , "untilReplied" .= untilRepliedTime
            , "untilResponded" .= untilRespondedTime
            ]

instance DefaultOrdered (Durations a) where
    headerOrder _ = header
        [ "untilParsed"
        , "untilEnqueued"
        , "untilDequeued"
        , "untilAsked"
        , "untilReplied"
        , "untilResponded"
        ]

instance Num a => Monoid (Durations a) where
    mempty = Durations
        { untilParsedTime    = 0
        , untilEnqueuedTime  = 0
        , untilDequeuedTime  = 0
        , untilAskedTime     = 0
        , untilRepliedTime   = 0
        , untilRespondedTime = 0
        }
    mappend d1 d2 =
        let s func = func d1 + func d2
        in Durations
            { untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }

instance (Num a, Divisive a) => Mean (Durations a) where
    combines ds =
        let s func = sum $ map func ds
        in Durations
            { untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }
    divide d i =
        let s func = func d `divi` i
        in Durations
            { untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }
    uncombine d1 d2 =
        let s func = func d1 - func d2
        in Durations
            { untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }

class Divisive a where
    divi :: Integral i => a -> i -> a

instance Divisive Integer where
    divi a i = a `div` fromIntegral i

instance Divisive Float where
    divi a i = a / fromIntegral i

