module AslBuild.Models.MM1.Report where

import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Experiment

import           AslBuild.Models.Utils


mm1ModelTexFile :: ExperimentConfig a => a -> FilePath
mm1ModelTexFile ecf = reportsTmpDir </> experimentTarget ecf ++ "-mm1" <.> texExt

mm1ModelFileForReport :: ExperimentConfig a => a -> Int -> FilePath
mm1ModelFileForReport ecf = (mm1ModelTexFile ecf `modelFileForReport`)

