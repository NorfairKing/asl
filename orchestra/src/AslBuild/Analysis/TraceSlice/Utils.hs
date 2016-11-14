module AslBuild.Analysis.TraceSlice.Utils where

import           System.IO

import qualified Data.Vector                        as V

import qualified Statistics.Sample                  as S

import           Pipes                              ((>->))
import qualified Pipes.ByteString                   as PB
import qualified Pipes.Csv                          as P
import qualified Pipes.Prelude                      as P

import           Development.Shake

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Types

avgDurations :: FilePath -> Action Durations
avgDurations path = do
    durs <- liftIO $ withFile path ReadMode $ \inHandle ->
        P.toListM $
                P.decodeByName (PB.fromHandle inHandle)
            >-> errorIgnorer
            >-> timeTransformer

    let s func = floor $ S.mean $ V.fromList $ map (fromIntegral . func) durs
    let dur = Durations
            { reqKind = READ -- Fixme
            , arrivalTime        = 0
            , untilParsedTime    = s untilParsedTime
            , untilEnqueuedTime  = s untilEnqueuedTime
            , untilDequeuedTime  = s untilDequeuedTime
            , untilAskedTime     = s untilAskedTime
            , untilRepliedTime   = s untilRepliedTime
            , untilRespondedTime = s untilRespondedTime
            }
    return dur
