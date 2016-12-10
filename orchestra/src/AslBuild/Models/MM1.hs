module AslBuild.Models.MM1 where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           System.IO

import           Development.Shake
import           Development.Shake.FilePath

import           Pipes                               (Pipe, (>->))
import qualified Pipes                               as P
import qualified Pipes.ByteString                    as PB
import qualified Pipes.Csv                           as P
import qualified Pipes.Prelude                       as P

import           AslBuild.Analysis.PipeUtils
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Client
import           AslBuild.Experiment
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Middleware
import           AslBuild.Utils

import           AslBuild.Models.MM1.Types

mm1Rule :: String
mm1Rule = "mm1-models"

mm1Rules :: Rules ()
mm1Rules = do
    mm1Rule ~> need [ruleForStabilityTraces]

    subRules
        mm1RulesFor
        ruleForStabilityTraces
        allStabilityTraceExperiments

ruleForStabilityTraces :: String
ruleForStabilityTraces = "stability-trace-mm1-models"

mm1RuleFor :: ExperimentConfig a => a -> String
mm1RuleFor ecf = experimentTarget ecf ++ "-mm1-model"

mm1MiddlewareModelFileFor :: ExperimentConfig a => a -> FilePath -> FilePath
mm1MiddlewareModelFileFor ecf = changeFilename (++ "-mm1-middleware") . (`replaceDirectory` experimentAnalysisTmpDir ecf)

mm1RulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
mm1RulesFor ecf = onlyIfResultsExist ecf $ do
    slocs <- readResultsSummaryLocationsForCfg ecf
    mm1ModelFiles <- forM slocs $ \sloc -> do
        let modelFile = mm1MiddlewareModelFileFor ecf sloc
        modelFile %> \outf -> do
            mm1Model <- calcMiddlewareMM1Model sloc
            liftIO $ print mm1Model
            writeJSON outf mm1Model
        return modelFile

    let mm1target = mm1RuleFor ecf
    mm1target ~> need mm1ModelFiles
    return mm1target

calcMiddlewareMM1Model :: MonadIO m => FilePath -> m MM1Model
calcMiddlewareMM1Model sloc = do
    ers <- readResultsSummary sloc
    case merMiddleResultsFile ers of
        Nothing -> fail "Cannot compute M/M/1 model without middleware trace."
        Just traceFile -> do
            setup <- readExperimentSetupForSummary ers
            arrAvg <- calcMiddlewareArrivalAvg setup traceFile
            serAvg <- calcMiddlewareServiceRateAvg traceFile
            pure $ MM1Model arrAvg serAvg

calcMiddlewareArrivalAvg :: MonadIO m => ExperimentSetup -> FilePath -> m Avg
calcMiddlewareArrivalAvg setup = do
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
    bucketizedInMiddlewareAverageOf (grequestsPerResultLine wp rsr wsr) requestReceivedTime

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
grequestsPerResultLine
    :: Double
        -- ^ Write percentage
    -> Int
        -- ^ Read sample rate
    -> Int
        -- ^ Write sample rate
    -> Double
grequestsPerResultLine wp rss wss =
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
            if s <= seccounter
            then help seccounter $ acc ++ [line]
            else do
                P.yield acc
                help s []

    sec :: Integer -> Integer
    sec = (`div` (1000 * 1000 * 1000))

    diff :: MiddleResultLine -> MiddleResultLine -> Integer
    diff m1 m2 = projection m2 - projection m1
