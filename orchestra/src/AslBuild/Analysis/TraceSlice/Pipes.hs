{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Analysis.TraceSlice.Pipes where

import           Control.Monad

import           Pipes                              (Pipe, (>->))
import qualified Pipes                              as P
import qualified Pipes.Prelude                      as P

import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Client.Types
import           AslBuild.Experiment
import           AslBuild.Memaslap.Types
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Utils


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

durTupLineTrans :: Monad m => Pipe (DurTup a) (DurationsLine a) m v
durTupLineTrans = forever $ do
    DurTup{..} <- P.await
    let row val cat = DurationsLine
            { nrCls = nrCs
            , middleThds = middleTds
            , category = cat
            , value = val durs
            }
    mapM_ P.yield
        [ row untilParsedTime     "Parsing"
        , row untilEnqueuedTime   "Waiting to be put onto queue"
        , row untilDequeuedTime   "In queue"
        , row untilAskedTime      "Querying first server"
        , row untilRepliedTime    "Interacting with server"
        , row untilRespondedTime  "Finalisation"
        ]

relDurTup :: DurTup Integer -> DurTup Float
relDurTup dt = dt { durs = relDur $ durs dt }

relDur :: Durations Integer -> Durations Float
relDur d = Durations
    { untilParsedTime    = rel untilParsedTime
    , untilEnqueuedTime  = rel untilEnqueuedTime
    , untilDequeuedTime  = rel untilDequeuedTime
    , untilAskedTime     = rel untilAskedTime
    , untilRepliedTime   = rel untilRepliedTime
    , untilRespondedTime = rel untilRespondedTime
    }
  where
    rel func = fromIntegral (func d) / fromIntegral (total d)
    total Durations{..} = sum
        [ untilParsedTime
        , untilEnqueuedTime
        , untilDequeuedTime
        , untilAskedTime
        , untilRepliedTime
        , untilRespondedTime
        ]

absLineTransformer :: Monad m => Pipe (DurTup Integer) (DurationsLine Integer) m v
absLineTransformer = durTupLineTrans

relLineTransformer :: Monad m => Pipe (DurTup Integer) (DurationsLine Float) m v
relLineTransformer = P.map relDurTup >-> durTupLineTrans

durtupTransformer :: Monad m => Pipe (ExperimentSetup, Durations a) (DurTup a) m v
durtupTransformer = P.map tr
  where tr (ExperimentSetup{..}, durations) = DurTup
            { nrCs             = sum $ map (msConcurrency . msFlags . cMemaslapSettings) clientSetups
            , middleTds        = mwNrThreads $ mMiddlewareFlags $ fst $ fromRight backendSetup
            , durs             = durations
            }
