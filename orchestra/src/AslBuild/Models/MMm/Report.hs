module AslBuild.Models.MMm.Report where

import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Experiment

import           AslBuild.Models.Utils

mmmModelTexFile :: ExperimentConfig a => a -> FilePath
mmmModelTexFile ecf = reportsTmpDir </> experimentTarget ecf ++ "-mmm" <.> texExt

mmmModelFileForReport :: ExperimentConfig a => a -> Int -> FilePath
mmmModelFileForReport ecf = (mmmModelTexFile ecf `modelFileForReport`)

