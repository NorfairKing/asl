{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AslBuild.Analysis.Trace.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Monoid
import GHC.Generics

import Data.Csv

import AslBuild.Types

data MiddleDurationsLine = MiddleDurationsLine
    { reqKind :: RequestKind
    , arrivalTime :: Integer
    , durations :: Durations Integer
    } deriving (Show, Eq, Generic)

instance FromNamedRecord MiddleDurationsLine where
    parseNamedRecord m =
        MiddleDurationsLine <$> m .: "reqKind" <*> m .: "arrivalTime" <*> parseNamedRecord m

instance ToNamedRecord MiddleDurationsLine where
    toNamedRecord MiddleDurationsLine {..} =
        namedRecord ["reqKind" .= reqKind, "arrivalTime" .= arrivalTime] <> toNamedRecord durations

instance DefaultOrdered MiddleDurationsLine where
    headerOrder _ =
        header ["reqKind", "arrivalTime"] <> headerOrder (undefined :: Durations Integer)

data Durations a = Durations
    { untilParsedTime :: a
    , untilEnqueuedTime :: a
    , untilDequeuedTime :: a
    , untilAskedTime :: a
    , untilRepliedTime :: a
    , untilRespondedTime :: a
    } deriving (Show, Eq, Generic)

instance Functor Durations where
    fmap f dur =
        Durations
        { untilParsedTime = f $ untilParsedTime dur
        , untilEnqueuedTime = f $ untilEnqueuedTime dur
        , untilDequeuedTime = f $ untilDequeuedTime dur
        , untilAskedTime = f $ untilAskedTime dur
        , untilRepliedTime = f $ untilRepliedTime dur
        , untilRespondedTime = f $ untilRespondedTime dur
        }

instance ToJSON a =>
         ToJSON (Durations a)

instance FromJSON a =>
         FromJSON (Durations a)

instance FromField a =>
         FromNamedRecord (Durations a) where
    parseNamedRecord m =
        Durations <$> m .: "untilParsed" <*> m .: "untilEnqueued" <*> m .: "untilDequeued" <*>
        m .: "untilAsked" <*>
        m .: "untilReplied" <*>
        m .: "untilResponded"

instance ToField a =>
         ToNamedRecord (Durations a) where
    toNamedRecord Durations {..} =
        namedRecord
            [ "untilParsed" .= untilParsedTime
            , "untilEnqueued" .= untilEnqueuedTime
            , "untilDequeued" .= untilDequeuedTime
            , "untilAsked" .= untilAskedTime
            , "untilReplied" .= untilRepliedTime
            , "untilResponded" .= untilRespondedTime
            ]

instance DefaultOrdered (Durations a) where
    headerOrder _ =
        header
            [ "untilParsed"
            , "untilEnqueued"
            , "untilDequeued"
            , "untilAsked"
            , "untilReplied"
            , "untilResponded"
            ]

binop :: (a -> b -> c) -> Durations a -> Durations b -> Durations c
binop func d1 d2 =
    Durations
    { untilParsedTime = untilParsedTime d1 `func` untilParsedTime d2
    , untilEnqueuedTime = untilEnqueuedTime d1 `func` untilEnqueuedTime d2
    , untilDequeuedTime = untilDequeuedTime d1 `func` untilDequeuedTime d2
    , untilAskedTime = untilAskedTime d1 `func` untilAskedTime d2
    , untilRepliedTime = untilRepliedTime d1 `func` untilRepliedTime d2
    , untilRespondedTime = untilRespondedTime d1 `func` untilRespondedTime d2
    }

constDur :: a -> Durations a
constDur i =
    Durations
    { untilParsedTime = i
    , untilEnqueuedTime = i
    , untilDequeuedTime = i
    , untilAskedTime = i
    , untilRepliedTime = i
    , untilRespondedTime = i
    }

instance Num a =>
         Num (Durations a) where
    (+) = binop (+)
    (-) = binop (-)
    (*) = binop (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = constDur . fromInteger

instance Fractional a =>
         Fractional (Durations a) where
    (/) = binop (/)
    fromRational = constDur . fromRational

instance Floating a =>
         Floating (Durations a) where
    pi = constDur pi
    (**) = binop (**)

totalDuration
    :: Num a
    => Durations a -> a
totalDuration Durations {..} =
    sum
        [ untilParsedTime
        , untilEnqueuedTime
        , untilDequeuedTime
        , untilAskedTime
        , untilRepliedTime
        , untilRespondedTime
        ]

class Divisive a where
    divi
        :: Integral i
        => a -> i -> a

instance Divisive Integer where
    divi a i = a `div` fromIntegral i

instance Divisive Float where
    divi a i = a / fromIntegral i

instance Divisive Double where
    divi a i = a / fromIntegral i

newtype TotalDuration = TotalDuration
    { unTotalDuration :: Integer
    }

instance ToNamedRecord TotalDuration where
    toNamedRecord td = namedRecord ["totalDuration" .= unTotalDuration td]

instance DefaultOrdered TotalDuration where
    headerOrder _ = header ["totalDuration"]

newtype ThinkTime = ThinkTime
    { unThinkTime :: Integer
    }

instance ToNamedRecord ThinkTime where
    toNamedRecord td = namedRecord ["thinkTime" .= unThinkTime td]

instance FromNamedRecord ThinkTime where
    parseNamedRecord m = ThinkTime <$> m .: "thinkTime"

instance DefaultOrdered ThinkTime where
    headerOrder _ = header ["thinkTime"]
