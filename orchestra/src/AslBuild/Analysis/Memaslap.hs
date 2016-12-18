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
import           AslBuild.Experiments.Extreme
import           AslBuild.Experiments.Factorial
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.ThinkTime
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
    rs5 <- mapM memaslapLogsRulesFor allThinkTimeExperiments
    rs6 <- mapM memaslapLogsRulesFor allFactorialExperiments
    rs7 <- mapM memaslapLogsRulesFor allExtremeExperiments
    memaslapLogsRule ~> need (catMaybes $ rs1 ++ rs2 ++ rs3 ++ rs4 ++ rs5 ++ rs6 ++ rs7)

memaslapLogRuleTargetFor :: ExperimentConfig a => a -> String
memaslapLogRuleTargetFor ecf = experimentTarget ecf ++ "-memaslap-logs"

memaslapLogsRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
memaslapLogsRulesFor ecf = onlyIfResultsExist ecf $ do
    slocss <- readResultsSummaryLocationsForCfg ecf
    resultsFiles <- forM slocss $ memaslapLogRulesForRepSet ecf

    let target = memaslapLogRuleTargetFor ecf
    target ~> need (concat resultsFiles)
    return target

memaslapLogRulesForRepSet :: ExperimentConfig a => a -> [FilePath] -> Rules [FilePath]
memaslapLogRulesForRepSet ecf slocs = do
    individualFss <- forM slocs $ memaslapLogsRulesForRep ecf
    let combinedRepsetResultsFile = combinedClientRepsetResultsFile ecf slocs
    combinedRepsetResultsFile %> \_ -> do
        let combinedResultsFiles = map (combineClientResultsFile ecf) slocs
        need combinedResultsFiles
        putLoud $ "Combining combined client result files " ++ show combinedResultsFiles ++ " into " ++ combinedRepsetResultsFile
        results <- mapM readCombinedClientResults combinedResultsFiles
        writeJSON combinedRepsetResultsFile $ combineClientsResults results

    pure $ combinedRepsetResultsFile : concat individualFss

combineClientsResults :: [MemaslapClientResults] -> CombinedClientResults
combineClientsResults mcrs = CombinedClientResults
    { avgTpsResults  = metaAvgResF tpsResults
    , avgRespResults = metaAvgResF respResults
    , avgMinTps      = mkAvgF minTps
    , avgMaxTps      = mkAvgF maxTps
    , avgMinResp     = mkAvgF maxResp
    , avgMaxResp     = mkAvgF maxResp
    }
  where
    mkAvgF :: (MemaslapClientResults -> Double) -> Avg
    mkAvgF func = mkAvg $ map func mcrs
    metaAvgResF :: (MemaslapClientResults -> AvgResults) -> MetaAvgResults
    metaAvgResF func =
        let as = map func mcrs
        in MetaAvgResults
            { avgGetResults  = metaAvg <$> mapM getResults as
            , avgSetResults  = metaAvg <$> mapM setResults as
            , avgBothResults = metaAvg $ map bothResults as
            }


readCombinedClientsResults :: MonadIO m => FilePath -> m CombinedClientResults
readCombinedClientsResults = readJSON

memaslapLogsRulesForRep :: ExperimentConfig a => a -> FilePath -> Rules [FilePath]
memaslapLogsRulesForRep ecf sloc = do
    ExperimentResultSummary{..} <- readResultsSummary sloc
    forM_ erClientLogFiles $ \logFile -> do
        let resultFile = localClientResultsFile ecf logFile
        resultFile %> \_ -> do
            need [logFile]
            putLoud $ "Parsing client log file " ++ logFile ++ " into " ++ resultFile
            mlog <- parseLog logFile
            case mlog of
                Nothing -> fail $ "could not parse logfile: " ++ logFile
                Just parsedLog -> writeJSON (localClientResultsFile ecf logFile) parsedLog

    let resultsFiles = map (localClientResultsFile ecf) erClientLogFiles
    let combinedResultsFile = combineClientResultsFile ecf sloc
    combinedResultsFile %> \_ -> do
        need resultsFiles
        putLoud $ "Combining client result files " ++ show resultsFiles ++ " into " ++ combinedResultsFile
        rs <- forP resultsFiles $ \resultsFile -> do
            clr <- readClientResults resultsFile
            let trips = triples clr
            let tprs = pureClientResults trips
            return tprs
        writeJSON combinedResultsFile $ combineClientResults rs

    return $ combinedResultsFile : resultsFiles

-- TODO fix this if we use a more complicated naming scheme for the reps
combinedClientRepsetResultsFile :: ExperimentConfig a => a -> [FilePath] -> FilePath
combinedClientRepsetResultsFile ecf fs
    = (`replaceSndDir` localClientResultsDir ecf)
    . changeFilename (const $ "combined-repset-results-" ++ takeBaseName (head fs))
    . head $ fs

combineClientResultsFile :: ExperimentConfig a => a -> FilePath -> FilePath
combineClientResultsFile ecf = (`replaceSndDir` localClientResultsDir ecf) . changeFilename (++ "-combined-results")

readCombinedClientResults :: MonadIO m => FilePath -> m MemaslapClientResults
readCombinedClientResults = readJSON

combineClientResults :: [MemaslapClientResults] -> MemaslapClientResults
combineClientResults cs = MemaslapClientResults
    { tpsResults  = combineTpsAvgResults $ map tpsResults cs
    , respResults = combineRespAvgResults $ map respResults cs
    , minTps = sum $ map minTps cs
    , maxTps = sum $ map maxTps cs -- Yes, take the sum, not the maximum.
    , minResp = minimum $ map minResp cs
    , maxResp = maximum $ map maxResp cs
    }

combineRespAvgResults :: [AvgResults] -> AvgResults
combineRespAvgResults rs = AvgResults
    { getResults  = combineRespAvgs <$> mapM getResults rs
    , setResults  = combineRespAvgs <$> mapM setResults rs
    , bothResults = combineRespAvgs $ map bothResults rs
    }

combineRespAvgs :: [Avg] -> Avg
combineRespAvgs as = Avg
    { avg = avgAvg $ map avg as
    , stdDev = combineStdDevs $ map stdDev as
    }

combineTpsAvgResults :: [AvgResults] -> AvgResults
combineTpsAvgResults rs = AvgResults
    { getResults  = combineTpsAvgs <$> mapM getResults rs
    , setResults  = combineTpsAvgs <$> mapM setResults rs
    , bothResults = combineTpsAvgs $ map bothResults rs
    }

combineTpsAvgs :: [Avg] -> Avg
combineTpsAvgs as = Avg
    { avg = sum $ map avg as
    , stdDev = combineStdDevs $ map stdDev as
    }

pureClientResults :: [StatsTriple] -> MemaslapClientResults
pureClientResults sts =
    let mkAvgS :: (Statistics -> Int) -> [StatisticsLog] -> Avg
        mkAvgS func sl = Avg { avg = S.mean vec, stdDev = S.stdDev vec }
          where vec = V.fromList $ map (fromIntegral . func . periodStats) sl
        mkAvgResults :: (Statistics -> Int) -> AvgResults
        mkAvgResults func = AvgResults
            { getResults = mkAvgS func <$> mapM getStats sts
            , setResults = mkAvgS func <$> mapM setStats sts
            , bothResults = mkAvgS func $ map bothStats sts
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

