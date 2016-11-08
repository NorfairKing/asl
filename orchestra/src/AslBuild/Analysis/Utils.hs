module AslBuild.Analysis.Utils where

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
