{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice where

import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.Generics
import           System.Directory                    (doesFileExist)
import           System.IO

import           Development.Shake                   hiding (doesFileExist)
import           Development.Shake.FilePath

import           Pipes                               (Pipe, (>->))
import qualified Pipes                               as P
import qualified Pipes.ByteString                    as P
import qualified Pipes.Csv                           as P

import           Data.Csv

import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Types

data TraceSliceAnalysisCfg
    = TraceSliceAnalysisCfg
    { tsaInput      :: FilePath
    , tsaSimplified :: FilePath
    } deriving (Show, Eq)

allTraceSlicePlots :: [FilePath]
allTraceSlicePlots = []

allTraceSliceAnalyses :: [TraceSliceAnalysisCfg]
allTraceSliceAnalyses = []

traceSliceAnalysisRule :: String
traceSliceAnalysisRule = "trace-slice-analysis"

traceSliceAnalysisCfgRule :: ExperimentConfig a => a -> String
traceSliceAnalysisCfgRule cfg = experimentTarget cfg ++ "-trace-slice-analysis"

configFromStabilityTrace :: StabilityTraceCfg -> Rules ()
configFromStabilityTrace stc@StabilityTraceCfg{..} = do
    let rslf = resultSummariesLocationFile stc
    exists <- liftIO $ doesFileExist rslf
    when exists $ do
        summaryPaths <- readResultsSummaryLocations rslf
        forM_ summaryPaths $ \summaryPath -> do
            ExperimentResultSummary{..} <- readResultsSummary summaryPath
            durationsFile erMiddleResultsFile %> \outFile ->
                liftIO $
                    withFile erMiddleResultsFile ReadMode $ \inHandle ->
                        withFile outFile WriteMode $ \outHandle ->
                            P.runEffect $
                                    P.decodeByName (P.fromHandle inHandle)
                                >-> errorLogger
                                >-> timeTransformer
                                >-> P.encodeByName durationsHeader
                                >-> P.toHandle outHandle
            want[durationsFile erMiddleResultsFile]

durationsFile :: FilePath -> FilePath
durationsFile f = dropExtensions f ++ "-durations" <.> csvExt

data Durations
    = Durations
    { reqKind            :: RequestKind
    , untilParsedTime    :: Integer
    , untilEnqueuedTime  :: Integer
    , untilDequeuedTime  :: Integer
    , untilAskedTime     :: Integer
    , untilRepliedTime   :: Integer
    , untilRespondedTime :: Integer
    } deriving (Show, Eq, Generic)

instance ToNamedRecord Durations where
    toNamedRecord Durations{..} =
        namedRecord
            [ "reqKind" .= reqKind
            , "untilParsed" .= untilParsedTime
            , "untilEnqueued" .= untilEnqueuedTime
            , "untilDequeued" .= untilDequeuedTime
            , "untilAsked" .= untilAskedTime
            , "untilReplied" .= untilRepliedTime
            , "untilResponded" .= untilRespondedTime
            ]

durationsHeader :: Header
durationsHeader = header
    [ "reqKind"
    , "untilParsed"
    , "untilEnqueued"
    , "untilDequeued"
    , "untilAsked"
    , "untilReplied"
    , "untilResponded"
    ]

errorLogger :: MonadIO m => Pipe (Either String a) a m v
errorLogger = forever $ do
    eea <- P.await
    case eea of
        Left err -> liftIO $ putStrLn err
        Right res -> P.yield res

timeTransformer :: Monad m => Pipe MiddleResultLine Durations m v
timeTransformer = forever $ do
    MiddleResultLine{..} <- P.await
    P.yield Durations
        { reqKind = requestKind
        , untilParsedTime    = requestParsedTime     - requestReceivedTime
        , untilEnqueuedTime  = requestEnqueuedTime   - requestParsedTime
        , untilDequeuedTime  = requestDequeuedTime   - requestEnqueuedTime
        , untilAskedTime     = requestAskedTime      - requestDequeuedTime
        , untilRepliedTime   = requestRepliedTime    - requestRepliedTime
        , untilRespondedTime = requestRespondedTime  - requestRespondedTime
        }


traceSliceAnalysisRules :: Rules ()
traceSliceAnalysisRules = do
    traceSliceAnalysisRule ~> need allTraceSlicePlots

    configFromStabilityTrace smallLocalStabilityTrace
    configFromStabilityTrace localStabilityTrace
    configFromStabilityTrace smallRemoteStabilityTrace
    configFromStabilityTrace remoteStabilityTrace


