{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Pipes where

import           Control.Monad

import           Pipes                              (Pipe)
import qualified Pipes                              as P
import qualified Pipes.Prelude                      as P

import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Client.Types
import           AslBuild.Experiment
import           AslBuild.Memaslap.Types
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Utils


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

absLineTransformer :: Monad m => Pipe DurTup (DurationsLine Integer) m v
absLineTransformer = forever $ do
    DurTup{..} <- P.await
    let row val cat = DurationsLine
            { nrCls = nrCs
            , middleThds = middleTds
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
            , middleThds = middleTds d
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
            , middleTds        = mwNrThreads $ mMiddlewareFlags $ fst $ fromRight backendSetup
            , tilParsedTime    = untilParsedTime
            , tilEnqueuedTime  = untilEnqueuedTime
            , tilDequeuedTime  = untilDequeuedTime
            , tilAskedTime     = untilAskedTime
            , tilRepliedTime   = untilRepliedTime
            , tilRespondedTime = untilRespondedTime
            }
