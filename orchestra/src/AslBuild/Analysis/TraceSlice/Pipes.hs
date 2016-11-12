{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Pipes where

import           Control.Monad

import           Pipes                              (Pipe)
import qualified Pipes                              as P

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

meanTransformer :: Monad m => Integer -> Pipe Durations Durations m ()
meanTransformer = slidingMean

instance Mean Durations where
    combines ds =
        let s func = sum $ map func ds
        in Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = arrivalTime $ last ds
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }
    divide d i =
        let s func = func d `div` fromIntegral i
        in Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = arrivalTime d
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }
    uncombine d1 d2 =
        let s func = func d1 - func d2
        in Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = arrivalTime d1
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }
    combine d1 d2 =
        let s func = func d1 + func d2
        in Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = arrivalTime d2
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
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
        , row "total"     $ sum
            [ untilParsedTime
            , untilEnqueuedTime
            , untilDequeuedTime
            , untilAskedTime
            , untilRepliedTime
            , untilRespondedTime
            ]
        ]

