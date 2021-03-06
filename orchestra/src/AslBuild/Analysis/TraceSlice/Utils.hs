module AslBuild.Analysis.TraceSlice.Utils where

import Control.Monad.IO.Class
import System.IO

import qualified Data.Csv as CSV
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.ByteString as PB
import qualified Pipes.Csv as P
import qualified Pipes.Prelude as P

import AslBuild.Analysis.PipeUtils
import AslBuild.Analysis.Trace
import AslBuild.Analysis.TraceSlice.Pipes
import AslBuild.Analysis.TraceSlice.Types
import AslBuild.Analysis.Types
import AslBuild.Experiment
import AslBuild.Experiments.MaximumThroughput

buildAvgDursFile
    :: MonadIO m
    => MaximumThroughputCfg -> FilePath -> m ()
buildAvgDursFile ecf outFile
    -- TODO fix this because now we're not combining the repititions first.
 = do
    summaryPaths <- concat <$> readResultsSummaryLocationsForCfg ecf
    liftIO $
        withFile outFile WriteMode $ \outHandle ->
            P.runEffect $
            P.each summaryPaths >-> P.mapM (oneTup ecf) >-> filterMaybes >-> durtupTransformer >->
            P.encodeByName (CSV.headerOrder (undefined :: DurTup Integer)) >->
            PB.toHandle outHandle

oneTup
    :: MonadIO m
    => MaximumThroughputCfg -> FilePath -> m (Maybe (ExperimentSetup, Durations Integer))
oneTup mtc summaryPath = do
    ers <- readResultsSummary summaryPath
    case merMiddleResultsFile ers of
        Nothing -> return Nothing
        Just erMiddleResultsFile -> do
            setup <- readExperimentSetupForSummary ers
            avgDur <- readAvgDurationsFile $ avgDurationFile mtc erMiddleResultsFile
            return $ Just (setup, fmap (round . avg) avgDur)
