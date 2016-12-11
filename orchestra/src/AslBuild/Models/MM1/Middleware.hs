module AslBuild.Models.MM1.Middleware where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           System.IO

import           Development.Shake.FilePath

import           Pipes                       (Pipe, (>->))
import qualified Pipes                       as P
import qualified Pipes.ByteString            as PB
import qualified Pipes.Csv                   as P
import qualified Pipes.Prelude               as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Client
import           AslBuild.Experiment
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Utils

import           AslBuild.Models.MM1.Types

mm1MiddlewareModelFileFor :: ExperimentConfig a => a -> FilePath -> FilePath
mm1MiddlewareModelFileFor ecf = changeFilename (++ "-mm1-middleware") . (`replaceDirectory` experimentAnalysisTmpDir ecf)

calcMiddlewareMM1Model :: MonadIO m => FilePath -> m MM1Model
calcMiddlewareMM1Model sloc = do
    ers <- readResultsSummary sloc
    setup <- readExperimentSetupForSummary ers
    traceFile <- case merMiddleResultsFile ers of
        Nothing -> fail "Cannot compute M/M/1 model without middleware trace."
        Just tf -> pure tf
    (ms, ss) <- case backendSetup setup of
        Left _ -> fail "Cannot compute M/M/1 model without middleware setup."
        Right tup -> pure tup
    arrAvg <- calcMiddlewareArrivalAvg setup traceFile
    serAvg <- calcMiddlewareServiceRateAvg traceFile
    -- Adjust for the problem with multiple workers
    let correction (Avg a s) = Avg (a * genericLength ss * fromIntegral (mwNrThreads (mMiddlewareFlags ms) + 1)) s
    pure $ MM1Model arrAvg (correction serAvg)

calcMiddlewareArrivalAvg :: MonadIO m => ExperimentSetup -> FilePath -> m Avg
calcMiddlewareArrivalAvg setup traceFile = do
    -- We need write percentage and sample rates.
    let wps = map (setProportion . msConfig . cMemaslapSettings) $ clientSetups setup
    unless (length (nub wps) == 1) $ fail "Clients used didn't all use the same write percentage."
    let wp = head wps
    middle <- case backendSetup setup of
        Left _ -> fail "Need a middleware setup to calculate arrival rate."
        Right (middle, _) -> pure middle
    let mwf = mMiddlewareFlags middle
    (rsr, wsr) <- case (,) <$> mwReadSampleRate mwf <*> mwWriteSampleRate mwf of
        Nothing -> fail "need to predefine sample rates to get arrival rate."
        Just t -> pure t
    let correctionFactor = requestsPerResultLine wp rsr wsr
    bucketizedInMiddlewareAverageOf correctionFactor requestReceivedTime traceFile

calcMiddlewareServiceRateAvg :: MonadIO m => FilePath -> m Avg
calcMiddlewareServiceRateAvg = averageInMiddlewareOf $ \mrl ->
    1 / (fromIntegral (requestRepliedTime mrl - requestDequeuedTime mrl) / (1000 * 1000 * 1000))

averageInMiddlewareOf :: MonadIO m => (MiddleResultLine -> Double) -> FilePath -> m Avg
averageInMiddlewareOf projection = avgInMiddlewareWithFold $ P.map projection

bucketizedInMiddlewareAverageOf :: MonadIO m => Double -> (MiddleResultLine -> Integer) -> FilePath -> m Avg
bucketizedInMiddlewareAverageOf correctionFactor projection
    = avgInMiddlewareWithFold $ bucketizer projection >-> P.map ((* correctionFactor) . genericLength)


-- If we find a write line, that means wss writes have been processed
-- If we see a read line, that means rss reads have been processed.
--
-- For every line, there are wp write lines and rp read lines, so wp * wss writes and rp * rss
-- reads have been processed.
requestsPerResultLine
    :: Double
        -- ^ Write percentage
    -> Int
        -- ^ Read sample rate
    -> Int
        -- ^ Write sample rate
    -> Double
requestsPerResultLine wp rss wss =
    let rp = 1 - wp -- read percentage
    in (wp * fromIntegral wss) + (rp * fromIntegral rss)

avgInMiddlewareWithFold :: MonadIO m => Pipe MiddleResultLine Double IO () -> FilePath -> m Avg
avgInMiddlewareWithFold pipe traceFile = do
    let withFold fold = liftIO $ withFile traceFile ReadMode $ \inHandle -> do
            let prod =
                        P.decodeByName (PB.fromHandle inHandle)
                    >-> errorIgnorer
                    >-> pipe

            fold prod
    a <- withFold $ \prod -> do
        let goa (n, tot) v = (n + 1, tot + v)

        (n, tot) <- P.fold goa (0 :: Integer, 0) id prod
        let a = tot / fromIntegral n
        pure a

    s <- withFold $ \prod -> do
        let gos (n, sumsqe) v = (n + 1, sumsqe + ((a - v) ** 2))

        (n, sumsqe) <- P.fold gos (0 :: Integer, 0) id prod
        let s = sqrt $ sumsqe / fromIntegral n
        pure s
    pure $ Avg a s

bucketizer :: Monad m => (MiddleResultLine -> Integer) -> Pipe MiddleResultLine [MiddleResultLine] m v
bucketizer projection = do
    first <- P.await
    go first
  where
    go first = help 0 []
      where
        help seccounter acc = do
            line <- P.await
            let s = sec $ diff first line
            -- If this line belongs in this bucket,
            if s <= seccounter
            -- Then add it to this bucket.
            then help seccounter $ acc ++ [line]
            -- Otherwise, yield the bucket we have currently
            else do
                P.yield acc
                -- Then increment the second counter
                let increment counter = do
                        -- If the line fits into the next bucket
                        let counter' = counter + 1
                        if s <= counter'
                        -- Then that's going to be the second counter
                        then pure counter'
                        -- Otherwise, yield an empty bucket here.
                        else do
                            P.yield []
                            increment counter'
                seccounter' <- increment seccounter
                -- Now that we've found the bucket that this line belongs in, put it in there.
                help seccounter' [line]

    sec :: Integer -> Integer
    sec = (`div` (1000 * 1000 * 1000))

    diff :: MiddleResultLine -> MiddleResultLine -> Integer
    diff m1 m2 = projection m2 - projection m1
