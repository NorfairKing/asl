{-# LANGUAGE OverloadedStrings #-}
module AslBuild.Analysis.Memaslap
    ( module AslBuild.Analysis.Memaslap
    , module AslBuild.Analysis.Memaslap.Types
    ) where

import qualified Data.Vector                      as V
import qualified Statistics.Sample                as S

import           Development.Shake

import           AslBuild.Analysis.Memaslap.Types
import           AslBuild.Analysis.Types
import           AslBuild.Experiment
import           AslBuild.Memaslap.Types


throughputResults :: [FilePath] -> Action MemaslapClientResults
throughputResults clrfs = do
    rs <- forP clrfs $ \clrf -> do
        clr <- readClientResults clrf
        let trips = triples clr
        let tprs = pureClientResults trips
        return tprs
    return $ combineClientResults rs

combineClientResults :: [MemaslapClientResults] -> MemaslapClientResults
combineClientResults cs = MemaslapClientResults
    { tpsResults  = combineAvgResults $ map tpsResults cs
    , respResults = combineAvgResults $ map respResults cs
    }

combineAvgResults :: [AvgResults] -> AvgResults
combineAvgResults rs = AvgResults
    { getResults  = combineTpsAvgs <$> mapM getResults rs
    , setResults  = combineTpsAvgs <$> mapM setResults rs
    , bothResults = combineTpsAvgs $ map bothResults rs
    }

combineTpsAvgs :: [Avg] -> Avg
combineTpsAvgs as = Avg
    { avg = sum $ map avg as
    , stdDev = sqrt $ sum $ map ((\x -> x*x) . stdDev) as
    }

pureClientResults :: [StatsTriple] -> MemaslapClientResults
pureClientResults sts =
    let mkAvg :: (Statistics -> Int) -> [StatisticsLog] -> Avg
        mkAvg func sl = Avg { avg = S.mean vec, stdDev = S.stdDev vec }
          where vec = V.fromList $ map (fromIntegral . func . periodStats) sl
        mkAvgResults :: (Statistics -> Int) -> AvgResults
        mkAvgResults func = AvgResults
            { getResults = mkAvg func <$> mapM getStats sts
            , setResults = mkAvg func <$> mapM setStats sts
            , bothResults = mkAvg func $ map bothStats sts
            }
    in MemaslapClientResults
        { tpsResults = mkAvgResults tps
        , respResults = mkAvgResults avgUs
        }

