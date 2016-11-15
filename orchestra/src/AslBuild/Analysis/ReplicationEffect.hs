{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.ReplicationEffect where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.ReplicationEffect.Types
import           AslBuild.Analysis.Throughput
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Types
import           AslBuild.Utils

replicationAnalysisRule :: String
replicationAnalysisRule = "replication-analysis"

replicationAnalysisRules :: Rules ()
replicationAnalysisRules = do
    replicationAnalysisRule ~> need
        [ ruleForReplicationEffects
        ]

    subRules
        rulesForReplicationAnalysis
        ruleForReplicationEffects
        allReplicationEffectExperiments

ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-replication-analysis"

ruleForReplicationAnalysis :: ExperimentConfig a => a -> String
ruleForReplicationAnalysis rec
    = experimentTarget rec ++ "-replication-analysis"

replicationAnalysisPlotsFor :: ReplicationEffectCfg -> [FilePath]
replicationAnalysisPlotsFor rec = do
    nrSers <- serverCounts rec
    return $ replicationAnalysisPrefixFor rec ++ "-" ++ show nrSers <.> pngExt

simplifiedReplicationCsv :: ReplicationEffectCfg -> FilePath
simplifiedReplicationCsv rec
    = experimentAnalysisTmpDir rec </> experimentTarget rec ++ "simplified-replication-analysis" <.> csvExt

replicationAnalysisPrefixFor :: ReplicationEffectCfg -> String
replicationAnalysisPrefixFor rec
    = experimentPlotsDir rec </> experimentTarget rec ++ "-replication-analysis"

-- useReplicationAnalysisPlotsInReport :: ExperimentConfig a => a -> Int -> Rules ()
-- useReplicationAnalysisPlotsInReport stc
--     = usePlotsInReport $ replicationAnalysisPlotsFor stc
--
-- dependOnReplicationAnalysisPlotsForReport :: ExperimentConfig a => a -> Int -> Action ()
-- dependOnReplicationAnalysisPlotsForReport stc
--     = dependOnPlotsForReport $ replicationAnalysisPlotsFor stc

replicationAnalysisScript :: FilePath
replicationAnalysisScript = analysisDir </> "analyze_replication_effect.r"

rulesForReplicationAnalysis :: ReplicationEffectCfg -> Rules (Maybe String)
rulesForReplicationAnalysis rec = onlyIfResultsExist rec $ do
    let plots = replicationAnalysisPlotsFor rec

    let simplifiedCsv = simplifiedReplicationCsv rec
    simplifiedCsv %> \outFile -> do
        slocs <- readResultsSummaryLocationsForCfg rec
        lines_ <- forP slocs $ \sloc -> do
            ers <- readResultsSummary sloc
            setup <- readExperimentSetupForSummary ers
            res <- throughputResults $ erClientResultsFiles ers
            simplifiedCsvLines setup res

        writeCSV outFile $ concat lines_

    plots &%> \_ -> do
        need [replicationAnalysisScript, commonRLib, rBin, simplifiedCsv]
        needRLibs ["ggplot2"]
        rScript replicationAnalysisScript commonRLib simplifiedCsv $ replicationAnalysisPrefixFor rec

    let analysisTarget = ruleForReplicationAnalysis rec
    analysisTarget ~> need plots
    return analysisTarget


simplifiedCsvLines :: ExperimentSetup -> ThroughputResults -> Action [SimplifiedCsvLine]
simplifiedCsvLines ExperimentSetup{..} ThroughputResults{..} = do
    gAvg <- case getThroughputResults of
        Nothing -> fail "Missing get throughput results."
        Just r -> return r
    sAvg <- case setThroughputResults of
        Nothing -> fail "Missing set throughput results."
        Just r -> return r
    let (ms, sss) = fromRight backendSetup
    let line k a = SimplifiedCsvLine
            { nrServers = length sss
            , replicationFactor = mwReplicationFactor $ mMiddlewareFlags ms
            , kind = k
            , tpsAvg = a
            }
    return
        [ line READ gAvg
        , line WRITE sAvg
        ]
