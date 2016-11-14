{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Pipes where

import           Control.Monad

import           Pipes                              (Pipe)
import qualified Pipes                              as P
import qualified Pipes.Prelude                      as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Client.Types
import           AslBuild.Experiment
import           AslBuild.Memaslap.Types
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

instance Monoid Durations where
    mempty = Durations
        { reqKind = READ -- Fixme
        , arrivalTime        = 0
        , untilParsedTime    = 0
        , untilEnqueuedTime  = 0
        , untilDequeuedTime  = 0
        , untilAskedTime     = 0
        , untilRepliedTime   = 0
        , untilRespondedTime = 0
        }
    mappend d1 d2 =
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

absLineTransformer :: Monad m => Pipe DurTup (DurationsLine Integer) m v
absLineTransformer = forever $ do
    DurTup{..} <- P.await
    let row val cat = DurationsLine
            { nrCls = nrCs
            , category = cat
            , value = val
            }
    mapM_ P.yield
        [ row tilParsedTime     "Parsing"
        , row tilEnqueuedTime   "Waiting to be put onto queue"
        , row tilDequeuedTime   "In queue"
        , row tilAskedTime      "Querying first server"
        , row tilRepliedTime    "Interacting with server"
        , row tilRespondedTime  "Finalisation"
        ]

relLineTransformer :: Monad m => Pipe DurTup (DurationsLine Float) m v
relLineTransformer = forever $ do
    d <- P.await
    let total = sum
            [ tilParsedTime     d
            , tilEnqueuedTime   d
            , tilDequeuedTime   d
            , tilAskedTime      d
            , tilRepliedTime    d
            , tilRespondedTime  d
            ]
    let rel :: (DurTup -> Integer) -> Float
        rel func = (fromIntegral (func d) / fromIntegral total) * 100
    let row val cat = DurationsLine
            { nrCls = nrCs d
            , category = cat
            , value = rel val
            }
    mapM_ P.yield
        [ row tilParsedTime     "Parsing"
        , row tilEnqueuedTime   "Waiting to be put onto queue"
        , row tilDequeuedTime   "In queue"
        , row tilAskedTime      "Querying first server"
        , row tilRepliedTime    "Interacting with server"
        , row tilRespondedTime  "Finalisation"
        ]


durtupTransformer :: Monad m => Pipe (ExperimentSetup, Durations) DurTup m v
durtupTransformer = P.map tr
  where tr (ExperimentSetup{..}, Durations{..}) = DurTup
            { nrCs             = sum $ map (msConcurrency . msFlags . cMemaslapSettings) clientSetups
            , tilParsedTime    = untilParsedTime
            , tilEnqueuedTime  = untilEnqueuedTime
            , tilDequeuedTime  = untilDequeuedTime
            , tilAskedTime     = untilAskedTime
            , tilRepliedTime   = untilRepliedTime
            , tilRespondedTime = untilRespondedTime
            }
