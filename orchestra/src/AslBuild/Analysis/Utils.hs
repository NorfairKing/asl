module AslBuild.Analysis.Utils where

import           Data.Maybe

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.CommonActions
import           AslBuild.Experiment


onlyIfResultsExist :: ExperimentConfig a => a -> Rules b -> Rules (Maybe b)
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
