module AslBuild.Models.MM1.Clients where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Utils

import           AslBuild.Models.MM1.Types

mm1ClientsModelFileFor :: ExperimentConfig a => a -> FilePath -> FilePath
mm1ClientsModelFileFor ecf = changeFilename (++ "-mm1-clients") . (`replaceDirectory` experimentAnalysisTmpDir ecf)

calcClientsMM1Model :: ExperimentConfig a => a -> FilePath -> Action MM1Model
calcClientsMM1Model ecf sloc = do
    let combinedResultsFile = combineClientResultsFile ecf sloc
    need [combinedResultsFile]
    res <- readCombinedClientResults combinedResultsFile
    let v = 105
    let λ = avg $ bothResults $ tpsResults res
    let μ = ((1 + v) * λ) / v
    let arrAvg = Avg λ (read "Infinity") -- Arrival rate is estimated as average throughput
    let serAvg = Avg μ (read "Infinity") -- Service rate is estimated as maximum throughput
    pure $ MM1Model arrAvg serAvg
