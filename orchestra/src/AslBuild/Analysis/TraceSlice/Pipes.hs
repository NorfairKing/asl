{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Pipes where

import           Control.Monad

import qualified Data.Vector                        as V

import           Pipes                              (Pipe, (>->))
import qualified Pipes                              as P
import qualified Pipes.Prelude                      as P
import qualified Statistics.Sample                  as S

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Experiment
import           AslBuild.Types


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

-- Chunk size
meanTransformer :: Monad m => Integer -> Pipe Durations Durations m v
meanTransformer chunkSize = windows chunkSize >-> P.map durMean
  where
    durMean :: [Durations] -> Durations
    durMean dats = Durations
        { reqKind = READ -- Fixme
        , arrivalTime        = arrivalTime $ head dats
        , untilParsedTime    = mean untilParsedTime
        , untilEnqueuedTime  = mean untilEnqueuedTime
        , untilDequeuedTime  = mean untilDequeuedTime
        , untilAskedTime     = mean untilAskedTime
        , untilRepliedTime   = mean untilRepliedTime
        , untilRespondedTime = mean untilRespondedTime
        }
      where
        vdats = V.fromList dats
        mean :: (Durations -> Integer) -> Integer
        mean func = floor $ S.mean $ V.map (fromIntegral . func) vdats

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
        , row "total"     $ sum
            [ untilParsedTime
            , untilEnqueuedTime
            , untilDequeuedTime
            , untilAskedTime
            , untilRepliedTime
            , untilRespondedTime
            ]
        ]
