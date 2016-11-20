{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.Memaslap
    ( module AslBuild.Analysis.Memaslap
    , module AslBuild.Analysis.Memaslap.Types
    ) where

import           Control.Monad
import           Data.Maybe
import qualified Data.Vector                            as V
import qualified Statistics.Sample                      as S

import           Development.Shake

import           AslBuild.Analysis.Memaslap.Types
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Memaslap.LogParser
import           AslBuild.Memaslap.Types
import           AslBuild.Utils

memaslapLogsRule :: String
memaslapLogsRule = "memaslap-logs"

memaslapLogsRules :: Rules ()
memaslapLogsRules = do
    rs1 <- mapM memaslapLogsRulesFor allMaximumThroughputExperiments
    rs2 <- mapM memaslapLogsRulesFor allReplicationEffectExperiments
    rs3 <- mapM memaslapLogsRulesFor allWriteEffectExperiments
    memaslapLogsRule ~> need (catMaybes $ rs1 ++ rs2 ++ rs3)

memaslapLogRuleTargetFor :: ExperimentConfig a => a -> String
memaslapLogRuleTargetFor ecf = experimentTarget ecf ++ "-memaslap-logs"

memaslapLogsRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
memaslapLogsRulesFor ecf = onlyIfResultsExist ecf $ do
    slocs <- readResultsSummaryLocationsForCfg ecf
    resultFiles <- forM slocs $ \sloc -> do
        ExperimentResultSummary{..} <- readResultsSummary sloc
        let resultsFiles = map (localClientResultsFile ecf) erClientLogFiles
        resultsFiles &%> \_ -> do
            need erClientLogFiles
            forP_ erClientLogFiles $ \logFile -> do
                mlog <- parseLog logFile
                case mlog of
                    Nothing -> fail $ "could not parse logfile: " ++ logFile
                    Just parsedLog -> writeJSON (localClientResultsFile ecf logFile) parsedLog
        return resultsFiles

    let target = memaslapLogRuleTargetFor ecf
    target ~> need (concat resultFiles)
    return target


throughputResults :: ExperimentConfig a => a -> [FilePath] -> Action MemaslapClientResults
throughputResults ecf clrfs = do
    let resultsFiles = map (localClientResultsFile ecf) clrfs
    need resultsFiles
    rs <- forP resultsFiles $ \resultsFile -> do
        clr <- readClientResults resultsFile
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

