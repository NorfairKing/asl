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
import qualified Pipes.ByteString                    as PB
import qualified Pipes.Csv                           as P
import qualified Pipes.Prelude                       as P

import           Data.Csv

import           AslBuild.Analysis.BuildR
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Types

data Durations
    = Durations
    { reqKind            :: RequestKind
    , arrivalTime        :: Integer
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
            , "arrivalTime" .= arrivalTime
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
    , "arrivalTime"
    , "untilParsed"
    , "untilEnqueued"
    , "untilDequeued"
    , "untilAsked"
    , "untilReplied"
    , "untilResponded"
    ]

data DurationsLine
    = DurationsLine
    { rKind    :: RequestKind
    , aTime    :: Integer
    , category :: String
    , value    :: Integer
    } deriving (Show, Eq, Generic)

instance ToNamedRecord DurationsLine where
    toNamedRecord DurationsLine{..} =
        namedRecord
            [ "rKind" .= rKind
            , "aTime" .= aTime
            , "category" .= category
            , "value" .= value
            ]

durationsLineHeader :: Header
durationsLineHeader = header
    [ "rKind"
    , "aTime"
    , "category"
    , "value"
    ]

errorLogger :: MonadIO m => Pipe (Either String a) a m v
errorLogger = forever $ do
    eea <- P.await
    case eea of
        Left err -> liftIO $ putStrLn err
        Right res -> P.yield res

timeTransformer :: Monad m => Pipe MiddleResultLine Durations m v
timeTransformer = do
    mrl <- P.await
    let startTime = requestReceivedTime mrl
    forever $ do
        MiddleResultLine{..} <- P.await
        P.yield Durations
            { reqKind = requestKind
            , arrivalTime        = requestReceivedTime   - startTime
            , untilParsedTime    = requestParsedTime     - requestReceivedTime
            , untilEnqueuedTime  = requestEnqueuedTime   - requestParsedTime
            , untilDequeuedTime  = requestDequeuedTime   - requestEnqueuedTime
            , untilAskedTime     = requestAskedTime      - requestDequeuedTime
            , untilRepliedTime   = requestRepliedTime    - requestRepliedTime
            , untilRespondedTime = requestRespondedTime  - requestRespondedTime
            }

meanTransformer :: Monad m => Pipe Durations Durations m v
meanTransformer = forever $ do
    let chunkSize :: Integer
        chunkSize = 250
    dats <- replicateM (fromIntegral chunkSize) P.await
    let mean :: (Durations -> Integer) -> Integer
        mean func = sum (map func dats) `div` chunkSize
    P.yield Durations
        { reqKind = READ -- Fixme
        , arrivalTime        = arrivalTime $ head dats
        , untilParsedTime    = mean untilParsedTime
        , untilEnqueuedTime  = mean untilEnqueuedTime
        , untilDequeuedTime  = mean untilDequeuedTime
        , untilAskedTime     = mean untilAskedTime
        , untilRepliedTime   = mean untilRepliedTime
        , untilRespondedTime = mean untilRespondedTime
        }

lineTransformer :: Monad m => Pipe Durations DurationsLine m v
lineTransformer = forever $ do
    Durations{..} <- P.await
    let row cat val = DurationsLine
            { rKind = reqKind
            , aTime = arrivalTime
            , category = cat
            , value = val
            }
    mapM_ P.yield
        [ row "parsing"   untilParsedTime
        , row "enqueue"   untilEnqueuedTime
        , row "inqueue"   untilDequeuedTime
        , row "query"     untilAskedTime
        , row "response"  untilRepliedTime
        , row "finalized" untilRespondedTime
        ]

traceSliceAnalysisRules :: Rules ()
traceSliceAnalysisRules = do
    traceSliceAnalysisRule ~> need allTraceSlicePlots

    configFromStabilityTrace smallLocalStabilityTrace
    configFromStabilityTrace localStabilityTrace
    configFromStabilityTrace smallRemoteStabilityTrace
    configFromStabilityTrace remoteStabilityTrace


allTraceSlicePlots :: [FilePath]
allTraceSlicePlots = []

traceSliceAnalysisRule :: String
traceSliceAnalysisRule = "trace-slice-analysis"

traceSliceAnalysisCfgRule :: ExperimentConfig a => a -> String
traceSliceAnalysisCfgRule cfg = experimentTarget cfg ++ "-trace-slice-analysis"

traceSliceAnalysisScript :: FilePath
traceSliceAnalysisScript = analysisDir </> "analyze_trace_slice.r"

traceSliceAnalysisOf :: FilePath -> FilePath -> Rules ()
traceSliceAnalysisOf outFile inFile = outFile %> \_ -> do
    need [traceSliceAnalysisScript, inFile]
    need [rBin]
    needRLibs ["ggplot2"]
    rScript traceSliceAnalysisScript inFile outFile

configFromStabilityTrace :: StabilityTraceCfg -> Rules ()
configFromStabilityTrace stc@StabilityTraceCfg{..} = do
    let rslf = resultSummariesLocationFile stc
    exists <- liftIO $ doesFileExist rslf
    when exists $ do
        summaryPaths <- readResultsSummaryLocations rslf
        forM_ summaryPaths $ \summaryPath -> do
            es@ExperimentResultSummary{..} <- readResultsSummary summaryPath
            let dFile = durationsFile erMiddleResultsFile

            dFile %> \outFile ->
                liftIO $
                    withFile erMiddleResultsFile ReadMode $ \inHandle ->
                        withFile outFile WriteMode $ \outHandle ->
                            P.runEffect $
                                    P.decodeByName (PB.fromHandle inHandle)
                                >-> errorLogger
                                >-> timeTransformer
                                >-> P.drop 10
                                >-> meanTransformer
                                >-> lineTransformer
                                >-> P.encodeByName durationsLineHeader
                                >-> PB.toHandle outHandle

            traceSlicePlotFor es `traceSliceAnalysisOf` dFile

traceSlicePlotFor :: ExperimentResultSummary -> FilePath
traceSlicePlotFor ExperimentResultSummary{..} = dropExtensions erMiddleResultsFile ++ "slice" <.> pngExt

durationsFile :: FilePath -> FilePath
durationsFile f = dropExtensions f ++ "-durations" <.> csvExt

traceSlicePlot :: FilePath -> FilePath
traceSlicePlot f = dropExtensions f ++ "-trace-slice-plot" <.> pngExt
