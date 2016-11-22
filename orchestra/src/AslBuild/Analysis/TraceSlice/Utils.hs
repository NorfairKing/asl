module AslBuild.Analysis.TraceSlice.Utils where

import           Control.Monad.IO.Class
import           System.IO

import qualified Data.Csv                               as CSV
import           Pipes                                  ((>->))
import qualified Pipes                                  as P
import qualified Pipes.ByteString                       as PB
import qualified Pipes.Csv                              as P
import qualified Pipes.Prelude                          as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.TraceSlice.Pipes
import           AslBuild.Analysis.TraceSlice.Types
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Utils

buildAvgDursFile :: MonadIO m => MaximumThroughputCfg -> FilePath -> m ()
buildAvgDursFile ecf outFile = do
    summaryPaths <- readResultsSummaryLocationsForCfg ecf
    liftIO $ withFile outFile WriteMode $ \outHandle ->
        P.runEffect $
                P.each summaryPaths
            >-> P.mapM (oneTup ecf)
            >-> filterMaybes
            >-> durtupTransformer
            >-> P.encodeByName (CSV.headerOrder (undefined :: DurTup Integer))
            >-> PB.toHandle outHandle

oneTup :: MonadIO m => MaximumThroughputCfg -> FilePath -> m (Maybe (ExperimentSetup, Durations Integer))
oneTup mtc summaryPath = do
    ers <- readResultsSummary summaryPath
    case merMiddleResultsFile ers of
        Nothing -> return Nothing
        Just erMiddleResultsFile -> do
            setup <- readExperimentSetupForSummary ers
            avgDur <- readJSON $ avgDurationFile mtc erMiddleResultsFile
            return $ Just (setup, avgDur)
