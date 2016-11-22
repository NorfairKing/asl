{-# LANGUAGE RecordWildCards #-}
module AslBuild.Analysis.Trace
    ( module AslBuild.Analysis.Trace
    , module AslBuild.Analysis.Trace.Types
    ) where

import           Control.Monad
import           Control.Monad.IO.Class
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
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Types
import           AslBuild.Utils

traceRules :: Rules ()
traceRules = do
    mapM_ durationsRulesFor allMaximumThroughputExperiments
    mapM_ durationsRulesFor allReplicationEffectExperiments
    mapM_ durationsRulesFor allWriteEffectExperiments

durationsRulesFor :: ExperimentConfig a => a -> Rules ()
durationsRulesFor ecf = void $ onlyIfResultsExist ecf $ do
    summaryPaths <- readResultsSummaryLocationsForCfg ecf
    erss <- forM summaryPaths readResultsSummary

    mapM_ (durationsRulesForExperimentResults ecf) erss

durationsRulesForExperimentResults :: ExperimentConfig a => a -> ExperimentResultSummary -> Rules ()
durationsRulesForExperimentResults ecf er =
    case merMiddleResultsFile er of
        Nothing -> pure ()
        Just erMiddleResultsFile ->
            durationsRulesForMiddleResults ecf erMiddleResultsFile

durationsRulesForMiddleResults :: ExperimentConfig a => a -> FilePath -> Rules ()
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

    avgDurationFile ecf erMiddleResultsFile `asAvgDurationFileOf` rawDurs
    avgReadDurationFile ecf erMiddleResultsFile `asAvgDurationFileOf` readDurs
    avgWriteDurationFile ecf erMiddleResultsFile `asAvgDurationFileOf` writeDurs

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

calcAvgDur :: MonadIO m => FilePath -> m (Durations Integer)
calcAvgDur path = do
    (sumdurs, i) <- liftIO $ withFile path ReadMode $ \inHandle -> do
        let prod =
                    P.decodeByName (PB.fromHandle inHandle)
                >-> errorIgnorer
                >-> P.map durations

        let go :: (Durations Integer, Int) -> Durations Integer -> (Durations Integer, Int)
            go (s, l) d = (s `mappend` d, l + 1)

        P.fold go (mempty :: Durations Integer, 0) id prod

    return $ sumdurs `divide` i




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
rawDurationsFile ecf = changeFilename (++ "-durations") . (`replaceDirectory` experimentAnalysisTmpDir ecf)

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
