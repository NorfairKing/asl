module AslBuild.Analysis.ThinkTime where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           System.IO

import           Development.Shake

import           Pipes                          (Pipe, (>->))
import qualified Pipes                          as P
import qualified Pipes.ByteString               as PB
import qualified Pipes.Csv                      as P
import qualified Pipes.Prelude                  as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Experiments.ThinkTime
import           AslBuild.Utils

thinkTimeRule :: String
thinkTimeRule = "think-time-analysis"

thinkTimeRules :: Rules ()
thinkTimeRules = do
    ttrs <- mapM thinkTimeRulesFor allThinkTimeExperiments
    thinkTimeRule ~> need (catMaybes ttrs)

thinkTimeRuleFor :: ExperimentConfig a => a -> String
thinkTimeRuleFor ecf = experimentTarget ecf ++ "-think-time-analysis"

thinkTimeRulesFor :: ThinkTimeCfg -> Rules (Maybe String)
thinkTimeRulesFor ecf = onlyIfResultsExist ecf $ do
    slocss <- readResultsSummaryLocationsForCfg ecf
    fss <- forM slocss $ \slocs -> do
        erss <- mapM readResultsSummary slocs
        erMiddleResultsFiles <- case mapM merMiddleResultsFile erss of
            Nothing  -> fail "Need middleware for thinktime."
            Just mes -> pure mes

        inds <- forM erMiddleResultsFiles $ \erMiddleResultsFile -> do
            let ttf = thinkTimeFile ecf erMiddleResultsFile
            ttf %> \_ -> do
                putLoud $ unwords ["Gathering think time data from", erMiddleResultsFile, "into", ttf]
                transformCsvFileAction erMiddleResultsFile ttf thinkTimeTransformer

            let attf = avgThinkTimeFile ecf erMiddleResultsFile
            attf %> \_ -> do
                need [ttf]
                putLoud $ unwords ["Gathering average think time data from", ttf, "into", attf]
                a <- calcAvgThinkTime ttf
                writeJSON attf a
            pure (ttf, attf)

        let mavgf = metaAvgThinkTimeFile ecf erMiddleResultsFiles
        mavgf %> \_ -> do
            let afs = map snd inds
            need afs
            putLoud $ unwords ["Gathering meta average think time data from", show afs, "into", mavgf]
            avgs <- mapM readJSON afs
            writeJSON mavgf $ metaAvg avgs

        pure $ mavgf : map fst inds ++ map snd inds

    let rule = thinkTimeRuleFor ecf
    rule ~> need (concat fss)
    pure rule

thinkTimeFile :: ExperimentConfig a => a -> FilePath -> FilePath
thinkTimeFile ecf = changeFilename (++ "-think-time") . rawDurationsFile ecf

avgThinkTimeFile :: ExperimentConfig a => a -> FilePath -> FilePath
avgThinkTimeFile ecf = changeFilename (++ "-avg-think-time") . rawDurationsFile ecf

metaAvgThinkTimeFile :: ExperimentConfig a => a -> [FilePath] -> FilePath
metaAvgThinkTimeFile ecf = changeFilename (++ "-meta-avg-think-time") . rawDurationsFile ecf . head

thinkTimeTransformer :: Monad m => Pipe MiddleResultLine ThinkTime m v
thinkTimeTransformer = do
    first <- P.await
    go first
  where
    go prev = do
        next <- P.await
        P.yield $ ThinkTime $ requestReceivedTime next - requestRespondedTime prev
        go next

calcAvgThinkTime :: MonadIO m => FilePath -> m Avg
calcAvgThinkTime path = do
    let withFold fold = liftIO $ withFile path ReadMode $ \inHandle -> do
             let prod =
                         P.decodeByName (PB.fromHandle inHandle)
                     >-> errorIgnorer
                     >-> P.map (fromIntegral . unThinkTime :: ThinkTime -> Double)

             fold prod
    a <- withFold $ \prod -> do
        let goa (n, tot) v = (n + 1, tot + v)

        (n, tot) <- P.fold goa (0 :: Integer, 0 :: Double) id prod
        let a = tot / fromIntegral n
        pure a

    s <- withFold $ \prod -> do
        let gos (n, sumsqe) v = (n + 1, sumsqe + ((a - v) ** 2))

        (n, sumsqe) <- P.fold gos (0 :: Integer, 0 :: Double) id prod
        let s = sqrt $ sumsqe / fromIntegral n
        pure s
    pure $ Avg a s
