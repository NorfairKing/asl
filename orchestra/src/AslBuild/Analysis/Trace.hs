{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis.Trace
    ( module AslBuild.Analysis.Trace
    , module AslBuild.Analysis.Trace.Types
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           System.IO

import           Development.Shake
import           Development.Shake.FilePath

import           Pipes                                  (Pipe, (>->))
import qualified Pipes                                  as P
import qualified Pipes.ByteString                       as PB
import qualified Pipes.Csv                              as P
import qualified Pipes.Prelude                          as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.Trace.Types
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.ThinkTime
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Types
import           AslBuild.Utils

traceRule :: String
traceRule = "durations"

traceRules :: Rules ()
traceRules = do
    dfs1 <- mapM durationsRulesFor allMaximumThroughputExperiments
    dfs2 <- mapM durationsRulesFor allReplicationEffectExperiments
    dfs3 <- mapM durationsRulesFor allStabilityTraceExperiments
    dfs4 <- mapM durationsRulesFor allWriteEffectExperiments
    dfs5 <- mapM durationsRulesFor allThinkTimeExperiments

    traceRule ~> need (catMaybes $ dfs1 ++ dfs2 ++ dfs3 ++ dfs4 ++ dfs5)

durationsRuleFor :: ExperimentConfig a => a -> String
durationsRuleFor ecf = experimentTarget ecf ++ "-durations"

durationsRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
durationsRulesFor ecf = onlyIfResultsExist ecf $ do
    summaryPaths <- readResultsSummaryLocationsForCfg ecf
    durfs <- forM summaryPaths $ durationsRulesForRepset ecf

    let target = durationsRuleFor ecf
    target ~> need (concat durfs)
    pure target

durationsRulesForRepset :: ExperimentConfig a => a -> [FilePath] -> Rules [FilePath]
durationsRulesForRepset ecf slocs = do
    erss <- forM slocs readResultsSummary
    durfs <- concat <$> mapM (durationsRulesForExperimentResults ecf) erss
    combined <- combinedDursFileRules ecf erss
    pure $ combined : durfs

combinedDursFileRules :: ExperimentConfig a => a -> [ExperimentResultSummary] -> Rules FilePath
combinedDursFileRules ecf erss =
    case mapM merMiddleResultsFile erss of
        Nothing -> fail "Need middles"
        Just mes -> do
            let combf = combinedAvgDurationFile ecf mes
            combf %> \_ -> do
                let adfss = map (avgDurationFile ecf) mes
                need adfss
                putLoud $ "Combining average durations of " ++ show adfss ++ " into " ++ combf
                avgDurss <- mapM readAvgDurationsFile adfss
                writeJSON combf $ combineAvgDurs avgDurss
            return combf

-- TODO also reads and writes if necessary
-- This needs the trace files, not the summary locations.
combinedAvgDurationFile :: ExperimentConfig a => a -> [FilePath] -> FilePath
combinedAvgDurationFile ecf =
      dropExtension -- Because now it's .json.csv
    . changeFilename (const "combined-durations")
    . rawDurationsFile ecf . head

readCombinedAvgDursFile :: MonadIO m => FilePath -> m (Durations MetaAvg)
readCombinedAvgDursFile = readJSON

combineAvgDurs :: [Durations Avg] -> Durations MetaAvg
combineAvgDurs das = Durations
    { untilParsedTime    = mkMetaAvg untilParsedTime
    , untilEnqueuedTime  = mkMetaAvg untilEnqueuedTime
    , untilDequeuedTime  = mkMetaAvg untilDequeuedTime
    , untilAskedTime     = mkMetaAvg untilAskedTime
    , untilRepliedTime   = mkMetaAvg untilRepliedTime
    , untilRespondedTime = mkMetaAvg untilRespondedTime
    }
  where mkMetaAvg func = metaAvg $ map func das


durationsRulesForExperimentResults :: ExperimentConfig a => a -> ExperimentResultSummary -> Rules [FilePath]
durationsRulesForExperimentResults ecf er =
    case merMiddleResultsFile er of
        Nothing -> pure []
        Just erMiddleResultsFile ->
            durationsRulesForMiddleResults ecf erMiddleResultsFile

durationsRulesForMiddleResults :: ExperimentConfig a => a -> FilePath -> Rules [FilePath]
durationsRulesForMiddleResults ecf erMiddleResultsFile = do
    let rawDurs = rawDurationsFile ecf erMiddleResultsFile
    rawDurs %> \outFile -> do
        putLoud $ unwords ["Gathering raw durations from", rawDurs, "into", outFile]
        transformCsvFileAction erMiddleResultsFile outFile timeTransformer

    let readDurs = readDurationsFile ecf erMiddleResultsFile
    readDurs %> \outFile -> do
        putLoud $ unwords ["Gathering read durations from", rawDurs, "into", outFile]
        transformCsvFileAction rawDurs outFile $ P.filter $ \mdl -> reqKind mdl == READ

    let writeDurs = writeDurationsFile ecf erMiddleResultsFile
    writeDurs %> \outFile -> do
        putLoud $ unwords ["Gathering write durations from", rawDurs, "into", outFile]
        transformCsvFileAction rawDurs outFile $ P.filter $ \mdl -> reqKind mdl == WRITE

    let totalDur = totalDurFile ecf erMiddleResultsFile
    totalDur %> \outFile -> do
        putLoud $ unwords ["Gathering total durations from", erMiddleResultsFile, "into", outFile]
        transformCsvFileAction erMiddleResultsFile outFile $ P.map $ \mrl ->
            TotalDuration $ requestRespondedTime mrl - requestReceivedTime mrl


    let avgDurF = avgDurationFile ecf erMiddleResultsFile
    avgDurF `asAvgDurationFileOf` rawDurs

    let avgReadDurF = avgReadDurationFile ecf erMiddleResultsFile
    avgReadDurF `asAvgDurationFileOf` readDurs

    let avgWriteDurF = avgWriteDurationFile ecf erMiddleResultsFile
    avgWriteDurF `asAvgDurationFileOf` writeDurs

    pure [readDurs, writeDurs, totalDur, avgDurF, avgReadDurF, avgWriteDurF]

asAvgDurationFileOf :: FilePath -> FilePath -> Rules ()
asAvgDurationFileOf avgFile durFile =
    avgFile %> \outFile -> do
        need [durFile]
        putLoud $ unwords ["Averaging durations of", durFile, "into", outFile]
        aDur <- calcAvgDur durFile
        writeJSON outFile aDur


timeTransformer :: Monad m => Pipe MiddleResultLine MiddleDurationsLine m v
timeTransformer = do
    mrl <- P.await
    let startTime = requestReceivedTime mrl
    forever $ do
        MiddleResultLine{..} <- P.await
        P.yield MiddleDurationsLine
            { reqKind = requestKind
            , arrivalTime = requestReceivedTime   - startTime
            , durations = Durations
                { untilParsedTime    = requestParsedTime     - requestReceivedTime
                , untilEnqueuedTime  = requestEnqueuedTime   - requestParsedTime
                , untilDequeuedTime  = requestDequeuedTime   - requestEnqueuedTime
                , untilAskedTime     = requestAskedTime      - requestDequeuedTime
                , untilRepliedTime   = requestRepliedTime    - requestAskedTime
                , untilRespondedTime = requestRespondedTime  - requestRepliedTime
                }
            }

readAvgDurationsFile :: MonadIO m => FilePath -> m (Durations Avg)
readAvgDurationsFile = readJSON

calcAvgDur :: MonadIO m => FilePath -> m (Durations Avg)
calcAvgDur path = do
    let withFold fold = liftIO $ withFile path ReadMode $ \inHandle -> do
             let prod =
                         P.decodeByName (PB.fromHandle inHandle)
                     >-> errorIgnorer
                     >-> P.map durations
                     >-> P.map (fmap (fromIntegral :: Integer -> Double))

             fold prod
    a <- withFold $ \prod -> do
        let goa (n, tot) v = (n + 1, tot + v)

        (n, tot) <- P.fold goa (0 :: Integer, 0 :: Durations Double) id prod
        let a = tot / fromIntegral n
        pure a

    s <- withFold $ \prod -> do
        let gos (n, sumsqe) v = (n + 1, sumsqe + ((a - v) ** 2))

        (n, sumsqe) <- P.fold gos (0 :: Integer, 0 :: Durations Double) id prod
        let s = sqrt $ sumsqe / fromIntegral n
        pure s
    pure $ avgOfDurs a s

avgOfDurs :: Durations Double -> Durations Double -> Durations Avg
avgOfDurs da dst = Durations
    { untilParsedTime = av untilParsedTime
    , untilEnqueuedTime  = av untilEnqueuedTime
    , untilDequeuedTime  = av untilDequeuedTime
    , untilAskedTime     = av untilAskedTime
    , untilRepliedTime   = av untilRepliedTime
    , untilRespondedTime = av untilRespondedTime
    }
  where av func = Avg (func da) (func dst)



-- The raw trace file is in
--  erMiddleResultsFile
--
-- The raw durations file will be in
-- rawDuractionsFile erMiddleResultsFile
--
-- The durations for reads will be in
-- readDurationsFile erMiddleResultsFile
--
-- The durations for writes will be in
-- writeDurationsFile erMiddleResultsFile


rawDurationsFile :: ExperimentConfig a => a -> FilePath -> FilePath
rawDurationsFile ecf = changeFilename (++ "-durations") . (`replaceSndDir` experimentAnalysisTmpDir ecf)

readDurationsFile :: ExperimentConfig a => a -> FilePath -> FilePath
readDurationsFile ecf = changeFilename (++ "-read") . rawDurationsFile ecf

writeDurationsFile :: ExperimentConfig a => a -> FilePath -> FilePath
writeDurationsFile ecf = changeFilename (++ "-write") . rawDurationsFile ecf

avgDurationFile :: ExperimentConfig a => a -> FilePath -> FilePath
avgDurationFile ecf = changeFilename (++ "-average") . (<.> jsonExt) . dropExtensions . rawDurationsFile ecf

avgReadDurationFile :: ExperimentConfig a => a -> FilePath -> FilePath
avgReadDurationFile ecf = changeFilename (++ "-read") . avgDurationFile ecf

avgWriteDurationFile :: ExperimentConfig a => a -> FilePath -> FilePath
avgWriteDurationFile ecf = changeFilename (++ "-write") . avgDurationFile ecf

totalDurFile :: ExperimentConfig a => a -> FilePath -> FilePath
totalDurFile ecf = changeFilename (++ "-total") . rawDurationsFile ecf
