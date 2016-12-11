module AslBuild.IRTL where

import           Control.Monad
import           Text.Printf

import           Development.Shake

import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.WriteEffect

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

irtlRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
irtlRulesFor ecf = onlyIfResultsExist ecf $ do
    let rule = irtlRuleFor ecf
    rule ~> do
        slocs <- readResultsSummaryLocationsForCfg ecf
        forM_ slocs $ \sloc -> do
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
                r = (/ (1000 * 1000)) $ avg $ bothResults $ respResults res
                x = avg $ bothResults $ tpsResults res
                z = ((fromIntegral n) / x) - r

            liftIO $ print (n, r, x)
            liftIO $ putStrLn $ printf "%.2f μs" (z * 1000 * 1000)
            liftIO $ print $ (((fromIntegral n) / x), r)
    return rule
