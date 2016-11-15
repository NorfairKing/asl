{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.ReplicationEffect where

import           Data.Monoid
import           GHC.Generics

import           Development.Shake
import           Development.Shake.FilePath

import           Data.Csv
import qualified Data.Vector                            as V
import qualified Statistics.Sample                      as S

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Memaslap
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

data SimplifiedCsvLine
    = SimplifiedCsvLine
    { nrServers         :: Int
    , replicationFactor :: Int
    , kind              :: RequestKind
    , tpsAvg            :: Avg
    } deriving (Show, Eq, Generic)

instance ToNamedRecord SimplifiedCsvLine where
    toNamedRecord SimplifiedCsvLine{..} = namedRecord
        [ "nrServers" .= nrServers
        , "replicationFactor" .= replicationFactor
        , "kind" .= kind
        ] <> toNamedRecord tpsAvg

instance DefaultOrdered SimplifiedCsvLine where
    headerOrder _ = header
        [ "nrServers"
        , "replicationFactor"
        , "kind"
        ] <> headerOrder (undefined :: Avg)

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

data ThroughputResults
    = ThroughputResults
    { getThroughputResults  :: Maybe Avg
    , setThroughputResults  :: Maybe Avg
    , bothThroughputResults :: Avg
    } deriving (Show, Eq, Generic)

data Avg
    = Avg
    { avg    :: Double
    , stdDev :: Double
    } deriving (Show, Eq, Generic)

instance ToNamedRecord Avg where
    toNamedRecord Avg{..} = namedRecord
        [ "avg" .= (floor avg :: Integer)
        , "std" .= stdDev
        ]

instance DefaultOrdered Avg where
    headerOrder _ = header ["avg", "std"]

combineTpsAvgs :: [Avg] -> Avg
combineTpsAvgs as = Avg
    { avg = sum $ map avg as
    , stdDev = sqrt $ sum $ map stdDev as
    }

pureThroughputResults :: [StatsTriple] -> ThroughputResults
pureThroughputResults sts =
    let avg :: [StatisticsLog] -> Avg
        avg sl = Avg { avg = S.mean vec, stdDev = S.stdDev vec }
          where vec = V.fromList $ map (fromIntegral . tps . periodStats) sl
    in ThroughputResults
        { getThroughputResults = avg <$> mapM getStats sts
        , setThroughputResults = avg <$> mapM setStats sts
        , bothThroughputResults = avg $ map bothStats sts
        }
