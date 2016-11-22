{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.ReplicationEffect where

import           Data.List

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.ReplicationEffect.Types
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Reports.Common
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
    arity <- ["id", "rev"]
    count <- case arity of
        "id" -> show <$> serverCounts rec
        "rev" -> (show . (round :: Double -> Int). (*2)) <$> replicationFactors rec
        _ -> fail "wut."
    return $ intercalate "-" [replicationAnalysisPrefixFor rec, arity, count] <.> pngExt

replicationCostAnalysisPlotsFor :: ReplicationEffectCfg -> [FilePath]
replicationCostAnalysisPlotsFor rec = do
    nrSers <- serverCounts rec
    knd <- ["read", "write"]
    return $ intercalate "-" [replicationCostAnalysisPrefixFor rec, show nrSers, knd] <.> pngExt

simplifiedReplicationCsv :: ReplicationEffectCfg -> FilePath
simplifiedReplicationCsv rec
    = experimentAnalysisTmpDir rec </> experimentTarget rec ++ "-simplified-replication-analysis" <.> csvExt

simplifiedReplicationCostCsv :: ReplicationEffectCfg -> FilePath
simplifiedReplicationCostCsv rec
    = experimentAnalysisTmpDir rec </> experimentTarget rec ++ "-simplified-replication-cost-analysis" <.> csvExt

replicationAnalysisPrefixFor :: ReplicationEffectCfg -> String
replicationAnalysisPrefixFor rec
    = experimentPlotsDir rec </> experimentTarget rec ++ "-replication-analysis"

replicationCostAnalysisPrefixFor :: ReplicationEffectCfg -> String
replicationCostAnalysisPrefixFor rec
    = experimentPlotsDir rec </> experimentTarget rec ++ "-replication-cost-analysis"

useReplicationEffectPlotsInReport :: ReplicationEffectCfg -> Int -> Rules ()
useReplicationEffectPlotsInReport rec
    = usePlotsInReport $ replicationAnalysisPlotsFor rec ++ replicationCostAnalysisPlotsFor rec

dependOnReplicationEffectPlotsForReport :: ReplicationEffectCfg -> Int -> Action ()
dependOnReplicationEffectPlotsForReport rec
    = dependOnPlotsForReport $ replicationAnalysisPlotsFor rec ++ replicationCostAnalysisPlotsFor rec

replicationAnalysisScript :: FilePath
replicationAnalysisScript = analysisDir </> "analyze_replication_effect.r"

replicationCostAnalysisScript :: FilePath
replicationCostAnalysisScript = analysisDir </> "analyze_replication_cost.r"

rulesForReplicationAnalysis :: ReplicationEffectCfg -> Rules (Maybe String)
rulesForReplicationAnalysis rec = onlyIfResultsExist rec $ do
    let plots1 = replicationAnalysisPlotsFor rec

    let simplifiedCsv = simplifiedReplicationCsv rec
    simplifiedCsv %> \outFile -> do
        slocs <- readResultsSummaryLocationsForCfg rec
        lines_ <- forP slocs $ \sloc -> do
            ers <- readResultsSummary sloc
            setup <- readExperimentSetupForSummary ers
            res <- throughputResults rec $ erClientLogFiles ers
            simplifiedCsvLines setup res

        writeCSV outFile $ concat lines_

    plots1 &%> \_ -> do
        need [replicationAnalysisScript, commonRLib, rBin, simplifiedCsv]
        needRLibs ["ggplot2"]
        rScript replicationAnalysisScript commonRLib simplifiedCsv $ replicationAnalysisPrefixFor rec

    let plots2 = replicationCostAnalysisPlotsFor rec

    let simplifiedCostCsv = simplifiedReplicationCostCsv rec
    simplifiedCostCsv %> \outFile -> makeCostCsvFile rec outFile

    plots2 &%> \_ -> do
        need [replicationCostAnalysisScript, commonRLib, rBin, simplifiedCostCsv]
        needRLibs ["ggplot2"]
        rScript replicationCostAnalysisScript commonRLib simplifiedCostCsv $ replicationCostAnalysisPrefixFor rec


    let analysisTarget = ruleForReplicationAnalysis rec
    analysisTarget ~> need (plots1 ++ plots2)
    return analysisTarget


simplifiedCsvLines :: ExperimentSetup -> MemaslapClientResults -> Action [SimplifiedCsvLine]
simplifiedCsvLines ExperimentSetup{..} MemaslapClientResults{..} = do
    let respR = respResults
    gAvg <- case getResults respR of
        Nothing -> fail "Missing get throughput results."
        Just r -> return r
    sAvg <- case setResults respR of
        Nothing -> fail "Missing set throughput results."
        Just r -> return r
    let (ms, sss) = fromRight backendSetup
        nrsers = length sss
        repfac = mwReplicationFactor $ mMiddlewareFlags ms
    let line k a = SimplifiedCsvLine
            { nrServers = nrsers
            , replicationFactor = repfac
            , replicationCoefficient = (fromIntegral repfac - 1) / (fromIntegral nrsers - 1)
            , kind = k
            , respAvg = a
            }
    return
        [ line READ gAvg
        , line WRITE sAvg
        ]

makeCostCsvFile :: ReplicationEffectCfg -> FilePath -> Action ()
makeCostCsvFile rec outFile = do
    slocs <- readResultsSummaryLocationsForCfg rec
    lines_ <- forP slocs $ \sloc -> do
        ers <- readResultsSummary sloc
        setup <- readExperimentSetupForSummary ers
        simplifiedCostCsvLines rec setup ers

    writeCSV outFile $ concat lines_

simplifiedCostCsvLines :: ReplicationEffectCfg -> ExperimentSetup -> ExperimentResultSummary -> Action [SimplifiedCostCsvLine]
simplifiedCostCsvLines rec ExperimentSetup{..} ExperimentResultSummary{..} = do
    erMiddleResultsFile <- case merMiddleResultsFile of
        Nothing -> fail "Missing middleware trace."
        Just r -> pure r
    let readDursFile = avgReadDurationFile rec erMiddleResultsFile
    let writeDursFile = avgWriteDurationFile rec erMiddleResultsFile
    need [readDursFile, writeDursFile]
    avgReadDurs <- readJSON readDursFile
    avgWriteDurs <- readJSON writeDursFile
    let (ms, sss) = fromRight backendSetup
    return $ concatMap
        (uncurry $ makeDurLines
            (length sss) -- Nr servers
            (mwReplicationFactor $ mMiddlewareFlags ms)) -- Replication factor
        [(READ, avgReadDurs), (WRITE, avgWriteDurs)]

makeDurLines :: Int -> Int -> RequestKind -> Durations Integer -> [SimplifiedCostCsvLine]
makeDurLines s r k Durations{..} =
    [ row untilParsedTime     "Parsing"
    , row untilEnqueuedTime   "Waiting to be put onto queue"
    , row untilDequeuedTime   "In queue"
    , row untilAskedTime      "Querying first server"
    , row untilRepliedTime    "Interacting with server"
    , row untilRespondedTime  "Finalisation"
    ]
  where
    row val cat = SimplifiedCostCsvLine
        { nrSs = s
        , rf = r
        , knd = k
        , time = val
        , category = cat
        }

