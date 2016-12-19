{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AslBuild.Models.MMm.Internal where

import Data.Monoid
import GHC.Generics

import Data.Csv
import qualified Data.Vector as V

import AslBuild.Analysis.Types
import AslBuild.Models.MMm.Types
import AslBuild.Utils

data SimplifiedReplicationCsvLine = SimplifiedReplicationCsvLine
    { mmmModel :: MMmModel
    , actualTps :: MetaAvg
    , actualResp :: MetaAvg
    } deriving (Show, Eq, Generic)

instance ToNamedRecord SimplifiedReplicationCsvLine where
    toNamedRecord SimplifiedReplicationCsvLine {..} =
        toNamedRecord mmmModel <> mapKeys ("resp" <>) (toNamedRecord actualResp) <>
        mapKeys ("tps" <>) (toNamedRecord actualTps)

instance DefaultOrdered SimplifiedReplicationCsvLine where
    headerOrder _ =
        headerOrder (undefined :: MMmModel) <>
        V.map ("resp" <>) (headerOrder (undefined :: MetaAvg)) <>
        V.map ("tps" <>) (headerOrder (undefined :: MetaAvg))

instance ToNamedRecord MMmModel where
    toNamedRecord mmm@MMmModel {..} =
        namedRecord
            [ "arrivalRate" .= arrivalRate
            , "serviceRate" .= serviceRate
            , "nrServers" .= nrServers
            , "trafficIntensity" .= mmmTrafficIntensity mmm
            , "meanResponseTime" .= mmmMeanResponseTime mmm
            , "stdDevResponseTime" .= mmmStdDevResponseTime mmm
            , "meanWaitingTime" .= mmmMeanWaitingTime mmm
            , "stdDevWaitingTime" .= mmmStdDevResponseTime mmm
            ]

instance DefaultOrdered MMmModel where
    headerOrder _ =
        header
            [ "arrivalRate"
            , "serviceRate"
            , "nrServers"
            , "trafficIntensity"
            , "meanResponseTime"
            , "stdDevResponseTime"
            , "meanWaitingTime"
            , "stdDevWaitingTime"
            ]

mmmTrafficIntensity :: MMmModel -> Double
mmmTrafficIntensity mmm = λ / (m * μ)
  where
    λ = arrivalRate mmm
    μ = serviceRate mmm
    m = fromIntegral $ nrServers mmm

mmmMeanResponseTime :: MMmModel -> Double
mmmMeanResponseTime mmm = (1 / μ) * (1 + (ρ / (m * (1 - ρ))))
  where
    ρ = mmmTrafficIntensity mmm
    μ = serviceRate mmm
    m = fromIntegral $ nrServers mmm

mmmStdDevResponseTime :: MMmModel -> Double
mmmStdDevResponseTime mmm = sqrt $ (1 / μ) * (1 + ((ρ * (2 - ρ)) / ((m ** 2) * ((1 - ρ) ** 2))))
  where
    ρ = mmmTrafficIntensity mmm
    μ = serviceRate mmm
    m = fromIntegral $ nrServers mmm

mmmMeanWaitingTime :: MMmModel -> Double
mmmMeanWaitingTime mmm = ρ / (m * μ * (1 - ρ))
  where
    ρ = mmmTrafficIntensity mmm
    μ = serviceRate mmm
    m = fromIntegral $ nrServers mmm

mmmStdDevWaitingTime :: MMmModel -> Double
mmmStdDevWaitingTime mmm = sqrt $ (ρ * (2 - ρ)) / ((m ** 2) * (μ ** 2) * ((1 - ρ) ** 2))
  where
    ρ = mmmTrafficIntensity mmm
    μ = serviceRate mmm
    m = fromIntegral $ nrServers mmm
