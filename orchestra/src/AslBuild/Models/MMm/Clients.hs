module AslBuild.Models.MMm.Clients where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Utils

import           AslBuild.Models.MM1.Clients
import           AslBuild.Models.MMm.Types
import           AslBuild.Models.MMm.Utils

mmmClientsModelFileFor :: ExperimentConfig a => a -> FilePath -> FilePath
mmmClientsModelFileFor ecf = changeFilename (++ "-mmm-clients") . (`replaceDirectory` experimentAnalysisTmpDir ecf)

calcClientsMMmModel :: ExperimentConfig a => a -> FilePath -> Action MMmModel
calcClientsMMmModel ecf sloc = do
    mm1 <- calcClientsMM1Model ecf sloc
    fromMM1WithSummaryLocation mm1 sloc

