{-# LANGUAGE RecordWildCards #-}

module AslBuild.Analysis.Trace where

import           Control.Monad

import           Development.Shake
import           Development.Shake.FilePath

import           Pipes                         (Pipe)
import qualified Pipes                         as P
import qualified Pipes.Prelude                 as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.Trace.Types
import           AslBuild.Experiment
import           AslBuild.Types

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


rawDurationsFile :: FilePath -> FilePath
rawDurationsFile = changeFilename (++ "-durations")

readDurationsFile :: FilePath -> FilePath
readDurationsFile = changeFilename (++ "-read") . rawDurationsFile

writeDurationsFile :: FilePath -> FilePath
writeDurationsFile = changeFilename (++ "-write") . rawDurationsFile

changeFilename :: (FilePath -> FilePath) -> FilePath -> FilePath
changeFilename func path = func file ++ exts
  where (file, exts) = splitExtensions path


durationsRulesFor :: ExperimentResultSummary -> Rules ()
durationsRulesFor er =
    case merMiddleResultsFile er of
        Nothing -> pure ()
        Just erMiddleResultsFile ->
            durationsRulesForMiddleResults erMiddleResultsFile

durationsRulesForMiddleResults :: FilePath -> Rules ()
durationsRulesForMiddleResults erMiddleResultsFile = do
    let rawDurs = rawDurationsFile erMiddleResultsFile
    rawDurs %> \outFile ->
        transformCsvFileAction rawDurs outFile timeTransformer

    let readDurs = readDurationsFile erMiddleResultsFile
    readDurs %> \outFile ->
        transformCsvFileAction readDurs outFile $ P.filter $ \mdl -> reqKind mdl == READ

    let writeDurs = readDurationsFile erMiddleResultsFile
    writeDurs %> \outFile ->
        transformCsvFileAction writeDurs outFile $ P.filter $ \mdl -> reqKind mdl == WRITE

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
                , untilRepliedTime   = requestRepliedTime    - requestRepliedTime
                , untilRespondedTime = requestRespondedTime  - requestRespondedTime
                }
            }
