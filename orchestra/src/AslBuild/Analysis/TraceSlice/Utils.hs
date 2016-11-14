module AslBuild.Analysis.TraceSlice.Utils where

import           Control.Monad.IO.Class
import           System.IO

import qualified Data.Csv                               as CSV
import           Pipes                                  ((>->))
import qualified Pipes                                  as P
import qualified Pipes.ByteString                       as PB
import qualified Pipes.Csv                              as P
import qualified Pipes.Prelude                          as P

import           Development.Shake

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput

buildAvgDursFile :: MaximumThroughputCfg -> FilePath -> Action ()
buildAvgDursFile ecf outFile = do
    summaryPaths <- readResultsSummaryLocationsForCfg ecf
    liftIO $ withFile outFile WriteMode $ \outHandle ->
        P.runEffect $
                P.each summaryPaths
            >-> P.mapM oneTup
            >-> filterMaybes
            >-> durtupTransformer
            >-> P.encodeByName (CSV.headerOrder (undefined :: DurTup))
            >-> PB.toHandle outHandle

oneTup :: MonadIO m => FilePath -> m (Maybe (ExperimentSetup, Durations))
oneTup summaryPath = do
    ers <- readResultsSummary summaryPath
    case merMiddleResultsFile ers of
        Nothing -> return Nothing
        Just erMiddleResultsFile -> do
            setup <- readExperimentSetupForSummary ers
            avgDur <- avgDurations erMiddleResultsFile
            return $ Just (setup, avgDur)

avgDurations :: MonadIO m => FilePath -> m Durations
avgDurations path = do
    liftIO $ print path
    (sumdurs, i) <- liftIO $ withFile path ReadMode $ \inHandle -> do
        let prod =
                    P.decodeByName (PB.fromHandle inHandle)
                >-> errorIgnorer
                >-> timeTransformer

        let go :: (Durations, Int) -> Durations -> (Durations, Int)
            go (s, l) d = (s `mappend` d, l + 1)

        P.fold go (mempty :: Durations, 0) id prod

    return $ sumdurs `divide` i
