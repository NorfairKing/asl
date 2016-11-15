{-# LANGUAGE OverloadedStrings #-}
module AslBuild.Analysis.Throughput
    ( module AslBuild.Analysis.Throughput
    , module AslBuild.Analysis.Throughput.Types
    ) where

import qualified Data.Vector                        as V
import qualified Statistics.Sample                  as S

import           Development.Shake

import           AslBuild.Analysis.Throughput.Types
import           AslBuild.Experiment
import           AslBuild.Memaslap.Types


throughputResults :: [FilePath] -> Action ThroughputResults
throughputResults clrfs = do
    rs <- forP clrfs $ \clrf -> do
        clr <- readClientResults clrf
        let log_ = crLog clr
        let trips = triples log_
        let tprs = pureThroughputResults trips
        return tprs
    return ThroughputResults
        { getThroughputResults = combineTpsAvgs <$> mapM getThroughputResults rs
        , setThroughputResults = combineTpsAvgs <$> mapM setThroughputResults rs
        , bothThroughputResults = combineTpsAvgs $ map bothThroughputResults rs
        }

combineTpsAvgs :: [Avg] -> Avg
combineTpsAvgs as = Avg
    { avg = sum $ map avg as
    , stdDev = sqrt $ sum $ map ((\x -> x*x) . stdDev) as
    }

pureThroughputResults :: [StatsTriple] -> ThroughputResults
pureThroughputResults sts =
    let mkAvg :: [StatisticsLog] -> Avg
        mkAvg sl = Avg { avg = S.mean vec, stdDev = S.stdDev vec }
          where vec = V.fromList $ map (fromIntegral . tps . periodStats) sl
    in ThroughputResults
        { getThroughputResults = mkAvg <$> mapM getStats sts
        , setThroughputResults = mkAvg <$> mapM setStats sts
        , bothThroughputResults = mkAvg $ map bothStats sts
        }

