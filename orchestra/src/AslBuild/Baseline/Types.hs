{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Baseline.Types where

import           Data.Aeson            (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy  as LB
import           Data.Csv
import           GHC.Generics

import           AslBuild.Client.Types
import           AslBuild.Memaslap
import           AslBuild.Server.Types


data BaselineExperimentRuleCfg
    = BaselineExperimentRuleCfg
    { target                       :: String
    , csvOutFile                   :: FilePath
    , localLogfile                 :: FilePath
    , maxNrClients                 :: Int
    , baselineExperimentsCacheFile :: FilePath
    , baselineLocation             :: BaselineLocation
    , baselineSetup                :: BaseLineSetup
    } deriving (Show, Eq, Generic)

-- TODO refactor all locations together
data BaselineLocation
    = BaselineLocal
    | BaselineRemote
    deriving (Show, Eq, Generic)

data BaseLineSetup
    = BaseLineSetup
    { repetitions         :: Int
    , runtime             :: Int
    , maxNrVirtualClients :: Int
    } deriving (Show, Eq, Generic)

data BaselineExperimentSetup
    = BaselineExperimentSetup
    { repetition   :: Int
    , clientSetups :: [ClientSetup]
    , serverSetup  :: ServerSetup
    } deriving (Show, Eq, Generic)

instance ToJSON   BaselineExperimentSetup
instance FromJSON BaselineExperimentSetup

data ExperimentResults
    = ExperimentResults
    { erSetup       :: BaselineExperimentSetup
    , erClientSetup :: ClientSetup
    , erMemaslapLog :: MemaslapLog
    , erClientIndex :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResults
instance FromJSON ExperimentResults

data ExperimentLogLine
    = ExperimentLogLine
    { eNrClients   :: Int
    , eClientIndex :: Int
    , eRep         :: Int
    , eThreads     :: Int
    , eConcurrency :: Int
    , eAvg         :: Int
    , eStd         :: Double
    , eTps         :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON ExperimentLogLine
instance FromJSON ExperimentLogLine

resultsCsv :: [ExperimentLogLine] -> LB.ByteString
resultsCsv = encodeByName $ header
    [ "nrClients"
    , "clientIndex"
    , "rep"
    , "threads"
    , "concurrency"
    , "avg"
    , "std"
    , "tps"
    ]

instance ToNamedRecord ExperimentLogLine where
    toNamedRecord ExperimentLogLine{..} =
        namedRecord
            [ "nrClients" .= eNrClients
            , "clientIndex" .= eClientIndex
            , "rep" .= eRep
            , "threads" .= eThreads
            , "concurrency" .= eConcurrency
            , "avg" .= eAvg
            , "std" .= eStd
            , "tps" .= eTps
            ]


makeLogLine :: ExperimentResults -> Maybe ExperimentLogLine
makeLogLine ExperimentResults{..} = do
    let BaselineExperimentSetup{..} = erSetup
        ClientSetup{..} = erClientSetup
        MemaslapSettings{..} = cMemaslapSettings
        MemaslapFlags{..} = msFlags
        MemaslapLog{..} = erMemaslapLog
    TotalStats{..} <- totalBothStats <$> totalStatsTrip
    return ExperimentLogLine
        { eNrClients = length clientSetups
        , eClientIndex = erClientIndex
        , eRep = repetition
        , eThreads = msThreads
        , eConcurrency = msConcurrency
        , eAvg = totalAvg
        , eStd = totalStd
        , eTps = finalTps finalStats
        }
