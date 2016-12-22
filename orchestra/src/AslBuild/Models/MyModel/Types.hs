{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AslBuild.Models.MyModel.Types where

import           GHC.Generics

import           Data.Aeson
import           Data.Monoid
import qualified Data.Text    as T
import qualified Data.Vector  as V

data MyModel = MyModel
    { overallArrivalRate  :: Double
    , acceptorServiceTime :: Double
    , getServiceTime      :: Double
    , getNrServers        :: Double
    , setServiceTime      :: Double
    , setIServiceTime     :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON MyModel

instance FromJSON MyModel

data MyModelSolution = MyModelSolution
    { systemResponseTime  :: Double
    , avgNumberOfRequests :: Double
    , utilisations        :: PerCenter Double
    , responseTimes       :: PerCenter Double
    , throughputs         :: PerCenter Double
    , nrRequests          :: PerCenter Double
    , nrVisits            :: PerCenter Double
    , serviceTimes        :: PerCenter Double
    } deriving (Show, Eq, Generic)

instance ToJSON MyModelSolution
instance FromJSON MyModelSolution

data PerCenter a = PerCenter
    { forAcceptor     :: a
    , forReader       :: a
    , forFirstWriter  :: a
    , forSecondWriter :: a
    } deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (PerCenter a)
instance FromJSON a => FromJSON (PerCenter a)

newtype ByOctaveMyModelSolution = ByOctaveMyModelSolution
    { unOctave :: MyModelSolution
    } deriving (Show, Eq, Generic)

instance FromJSON ByOctaveMyModelSolution where
    parseJSON (Object o) = ByOctaveMyModelSolution <$> do
        totresp <- o .: "totalResponseTime"
        avgnrreq <- o .: "avgNrRequests"
        utilis <- percenter o "utilisations"
        resps <- percenter o "responseTimes"
        tpss <- percenter o "throughputs"
        nrreqs <- percenter o "nrRequests"
        nrviss <- percenter o "nrVisits"
        serss <- percenter o "serviceTimes"
        pure MyModelSolution
            { systemResponseTime  = totresp
            , avgNumberOfRequests = avgnrreq
            , utilisations        = utilis
            , responseTimes       = resps
            , throughputs         = tpss
            , nrRequests          = nrreqs
            , nrVisits            = nrviss
            , serviceTimes        = serss
            }
      where
        percenter o_ key = do
            v <- o_ .: key
            case v of
                Array a -> case V.toList a of
                    (acc:read_:write1:write2:_) -> PerCenter <$> parseJSON acc <*> parseJSON read_ <*> parseJSON write1 <*> parseJSON write2
                    _ -> fail $ "Unable to parse ByOctaveMyModelSolution percenter array " <> T.unpack key
                _ -> fail $ "Unable to parse ByOctaveMyModelSolution percenter " <> T.unpack key
    parseJSON _ = fail "Unable to parse ByOctaveMyModelSolution"
