module AslBuild.Analysis.Utils where

import Data.Maybe
import qualified Data.Vector as V
import qualified Statistics.Sample as S

import Development.Shake
import Development.Shake.FilePath

import AslBuild.Analysis.Types
import AslBuild.CommonActions
import AslBuild.Experiment

onlyIfResultsExist
    :: ExperimentConfig a
    => a -> Rules b -> Rules (Maybe b)
onlyIfResultsExist ecf = onlyIfFileExists $ resultSummariesLocationFile ecf

experimentAnalysisDir
    :: ExperimentConfig a
    => a -> FilePath
experimentAnalysisDir a = experimentLocalTmpDir a </> "analysis"

experimentPlotsDir
    :: ExperimentConfig a
    => a -> FilePath
experimentPlotsDir a = experimentAnalysisDir a </> "plots"

experimentAnalysisTmpDir
    :: ExperimentConfig a
    => a -> FilePath
experimentAnalysisTmpDir a = experimentAnalysisDir a </> "tmp"

subRules :: (a -> Rules (Maybe String)) -> String -> [a] -> Rules ()
subRules func subRule ls = do
    rs <- catMaybes <$> mapM func ls
    subRule ~> need rs

metaAvg :: [Avg] -> MetaAvg
metaAvg avgs =
    let as = map avg avgs
    in MetaAvg
       { avgAvgs = S.mean $ V.fromList as
       , stdDevAvgs = S.stdDev $ V.fromList as
       , combStdDev = combineStdDevs $ map stdDev avgs
       }

mkAvg :: [Double] -> Avg
mkAvg ds =
    let vec = V.fromList ds
    in Avg {avg = S.mean vec, stdDev = S.stdDev vec}

avgAvg :: [Double] -> Double
avgAvg = S.mean . V.fromList

combineStdDevs :: [Double] -> Double
combineStdDevs = sqrt . sum . map (\x -> x * x)
