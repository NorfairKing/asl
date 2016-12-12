{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.Memaslap
    ( module AslBuild.Analysis.Memaslap
    , module AslBuild.Analysis.Memaslap.Types
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Vector                            as V
import qualified Statistics.Sample                      as S

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Memaslap.Types
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
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
    rs4 <- mapM memaslapLogsRulesFor allStabilityTraceExperiments
    memaslapLogsRule ~> need (catMaybes $ rs1 ++ rs2 ++ rs3 ++ rs4)

memaslapLogRuleTargetFor :: ExperimentConfig a => a -> String
memaslapLogRuleTargetFor ecf = experimentTarget ecf ++ "-memaslap-logs"

memaslapLogsRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
memaslapLogsRulesFor ecf = onlyIfResultsExist ecf $ do
    slocs <- readResultsSummaryLocationsForCfg ecf
    resultFiles <- forM (concat slocs) $ \sloc -> do
        ExperimentResultSummary{..} <- readResultsSummary sloc
        let resultsFiles = map (localClientResultsFile ecf) erClientLogFiles
        resultsFiles &%> \_ -> do
            need erClientLogFiles
            forP_ erClientLogFiles $ \logFile -> do
                mlog <- parseLog logFile
                case mlog of
                    Nothing -> fail $ "could not parse logfile: " ++ logFile
                    Just parsedLog -> writeJSON (localClientResultsFile ecf logFile) parsedLog

        let combinedResultsFile = combineClientResultsFile ecf sloc
        combinedResultsFile %> \_ -> do
            need resultsFiles
            rs <- forP resultsFiles $ \resultsFile -> do
                clr <- readClientResults resultsFile
                let trips = triples clr
                let tprs = pureClientResults trips
                return tprs
            writeJSON combinedResultsFile $ combineClientResults rs

        return $ combinedResultsFile : resultsFiles

    let target = memaslapLogRuleTargetFor ecf
    target ~> need (concat resultFiles)
    return target

combineClientResultsFile :: ExperimentConfig a => a -> FilePath -> FilePath
combineClientResultsFile ecf = (`replaceDirectory` localClientResultsDir ecf) . changeFilename (++ "-combined-results")

readCombinedClientResults :: MonadIO m => FilePath -> m MemaslapClientResults
readCombinedClientResults = readJSON

combineClientResults :: [MemaslapClientResults] -> MemaslapClientResults
combineClientResults cs = MemaslapClientResults
    { tpsResults  = combineAvgResults $ map tpsResults cs
    , respResults = combineAvgResults $ map respResults cs
    , minTps = sum $ map minTps cs
    , maxTps = sum $ map maxTps cs -- Yes, take the sum, not the maximum.
    , minResp = minimum $ map minResp cs
    , maxResp = maximum $ map maxResp cs
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
        tpss = map (fromIntegral . tps . periodStats . bothStats) sts
        maxtps = maximum tpss
        mintps = minimum tpss

        maxresp = maximum $ map (fromIntegral . maxUs . periodStats . bothStats) sts
        minresp = minimum $ map (fromIntegral . minUs . periodStats . bothStats) sts
    in MemaslapClientResults
        { tpsResults = mkAvgResults tps
        , respResults = mkAvgResults avgUs
        , maxTps = maxtps
        , minTps = mintps
        , maxResp = maxresp
        , minResp = minresp
        }

