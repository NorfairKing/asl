module AslBuild.IRTL where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.List
import           Data.Maybe
import           Path.IO
import           System.Directory
import           Text.Printf

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
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

totalDurationsScript :: FilePath
totalDurationsScript = analysisDir </> "total_durs_histo.r"

irtlRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
irtlRulesFor ecf = onlyIfResultsExist ecf $ do
    genfile <- irtlGenfileRulesFor ecf
    plot <- irtlPlotRulesFor ecf
    let rule = irtlRuleFor ecf
    rule ~> need [genfile, plot]
    return rule

irtlPlotRuleFor :: ExperimentConfig a => a -> String
irtlPlotRuleFor ecf = experimentTarget ecf ++ "-irtl-plot"

irtlPlotDir :: ExperimentConfig a => a -> FilePath
irtlPlotDir ecf = experimentPlotsDir ecf </> "total-duration-histos"

irtlPlotDirForReport :: Int -> FilePath
irtlPlotDirForReport i = reportPlotsDir i </> "total-duration-histos"

irtlPlotFor :: ExperimentConfig a => a -> FilePath
irtlPlotFor ecf = irtlPlotDir ecf </> experimentTarget ecf ++ "-histogram-0" <.> pngExt

listIrtlPlots :: (MonadIO m, MonadCatch m, ExperimentConfig a) => a -> m [FilePath]
listIrtlPlots ecf = fromMaybe [] <$> forgivingAbsence (map (irtlPlotDir ecf </>) <$> liftIO (listDirectory $ irtlPlotDir ecf))

useIrtlPlotInReport :: ExperimentConfig a => a -> Int -> Rules ()
useIrtlPlotInReport ecf i = do
    fsInDir <- liftIO $ listIrtlPlots ecf
    let fs = nub $ irtlPlotFor ecf : fsInDir
    forM_ fs $ \f -> (f `replaceDirectory` irtlPlotDirForReport i) `byCopying` f

dependOnIrtlPlotForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnIrtlPlotForReport ecf i = do
    fsInDir <- liftIO $ listIrtlPlots ecf
    let fs = nub $ irtlPlotFor ecf : fsInDir
    need $ map (`replaceDirectory` irtlPlotDirForReport i)  fs

irtlPlotRulesFor :: ExperimentConfig a => a -> Rules String
irtlPlotRulesFor ecf = do
    let plot = irtlPlotFor ecf
    plot %> \_ -> do
        need [totalDurationsScript, commonRLib, rBin]
        slocs <- readResultsSummaryLocationsForCfg ecf
        forM_ (indexed slocs) $ \(ix, sloc) -> do
            ers <- readResultsSummary sloc
            setup <- readExperimentSetupForSummary ers
            erMiddleResultsFile <- case merMiddleResultsFile ers of
                Nothing -> fail "Missing middleware trace"
                Just r -> pure r
            let totalDurF = totalDurFile ecf erMiddleResultsFile
            need [totalDurF]
            unit $ rScript totalDurationsScript commonRLib totalDurF
                $ changeFilename ((++ show ix) . reverse . drop 1 . reverse)
                $ dropExtensions $ irtlPlotFor ecf

    let rule = irtlPlotRuleFor ecf
    rule ~> need [plot]
    pure rule

irtlGenfileRuleFor :: ExperimentConfig a => a -> String
irtlGenfileRuleFor ecf = experimentTarget ecf ++ "-irtl-genfile"

irtlGenfileRulesFor :: ExperimentConfig a => a -> Rules String
irtlGenfileRulesFor ecf = do
    let genfile = irtlGenfileFor ecf
    genfile %> \_ -> do
        table <- makeIrtTable ecf
        writeFile' genfile table
    let rule = irtlGenfileRuleFor ecf
    pure rule

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

        let showdub = printf "%.2f"
        pure [show n, showdub xa, showdub $ mematime ra, showdub $ mematime (nd / xa)]-- , showdub $ mematime za]
    pure $ tabularWithHeader ["Users", "Avg Throughput tps", "Avg Response time ($\\mu s$)", "Estimated response time"] ls
