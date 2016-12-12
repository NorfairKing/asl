module AslBuild.IRTL where

import           Control.Monad
import           Text.Printf

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Reports.Common
import           AslBuild.Reports.Utils
import           AslBuild.Utils

irtlRule :: String
irtlRule = "irtl"

irtlRules :: Rules ()
irtlRules = do
    irtlRule ~> need
        [ ruleForReplicationEffects
        , ruleForWriteEffects
        , ruleForMaxixumThroughputs
        , ruleForStabilityTraces
        ]
    subRules
        irtlRulesFor
        ruleForMaxixumThroughputs
        allMaximumThroughputExperiments

    subRules
        irtlRulesFor
        ruleForReplicationEffects
        allReplicationEffectExperiments

    subRules
        irtlRulesFor
        ruleForWriteEffects
        allWriteEffectExperiments

    subRules
        irtlRulesFor
        ruleForStabilityTraces
        allStabilityTraceExperiments

ruleForMaxixumThroughputs :: String
ruleForMaxixumThroughputs = "maximum-throughput-irtl"

ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-irtl"

ruleForWriteEffects :: String
ruleForWriteEffects = "write-effect-irtl"

ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-irtl"

irtlRuleFor :: ExperimentConfig a => a -> String
irtlRuleFor ecf = experimentTarget ecf ++ "-irtl"

irtlGenfileFor :: ExperimentConfig a => a -> FilePath
irtlGenfileFor ecf = experimentAnalysisTmpDir ecf </> experimentTarget ecf ++ "-irtl-table.tex"

irtlGenfileForReport :: ExperimentConfig a => a -> Int -> FilePath
irtlGenfileForReport ecf i = irtlGenfileFor ecf `replaceDirectory` reportGenfileDir i

useIrtlGenfileInReport :: ExperimentConfig a => a -> Int -> Rules ()
useIrtlGenfileInReport ecf i = irtlGenfileForReport ecf i `byCopying` irtlGenfileFor ecf

dependOnIrtlGenfileForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnIrtlGenfileForReport ecf i = need [irtlGenfileForReport ecf i]

irtlRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
irtlRulesFor ecf = onlyIfResultsExist ecf $ do
    let genfile = irtlGenfileFor ecf
    genfile %> \_ -> do
        table <- makeIrtTable ecf
        writeFile' genfile table
    let rule = irtlRuleFor ecf
    rule ~> need [genfile]
    return rule

makeIrtTable :: ExperimentConfig a => a -> Action String
makeIrtTable ecf = do
    slocs <- readResultsSummaryLocationsForCfg ecf
    ls <- forM slocs $ \sloc -> do
        let combinedResultsFile = combineClientResultsFile ecf sloc
        need [combinedResultsFile]
        res <- readCombinedClientResults combinedResultsFile
        ers <- readResultsSummary sloc
        setup <- readExperimentSetupForSummary ers

        -- Response time R
        -- Think time Z
        -- Throughput X
        --
        -- Z = R - (N / X)
        let n = nrUsers setup
            nd = fromIntegral n
            ra = unmematime $ avg $ bothResults $ respResults res
            xa = avg $ bothResults $ tpsResults res
            za = (nd / xa) - ra

            rm = unmematime $ minResp res
            xm = maxTps res
            zm = (nd / xm) - rm

            unmematime = (/ (1000 * 1000))
            mematime = (* (1000 * 1000))

        -- liftIO $ print (n, ra, xa)
        -- liftIO $ putStrLn $ printf "%.2f μs" (za * 1000 * 1000)
        -- liftIO $ print (n, rm, xm)
        liftIO $ putStrLn $ printf "%.2f μs" (mematime zm)
        let showdub = printf "%.2f"
        pure [show n, showdub xa, showdub $ mematime ra, showdub $ mematime za]-- , showdub $ mematime za]
    pure $ tabularWithHeader ["Users", "Avg Throughput tps", "Avg Response time ($\\mu s$)", "Estimated Think time ($\\mu s$)"] ls
