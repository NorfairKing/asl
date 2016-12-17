module AslBuild.Models.MyModel where

import           Control.Monad
import           Control.Monad.IO.Class

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Models.MM1.Types
import           AslBuild.Models.MMm.Types
import           AslBuild.Models.MyModel.Types
import           AslBuild.Models.Utils
import           AslBuild.Reports.Common
import           AslBuild.Utils

myModelRule :: String
myModelRule = "my-models"

myModelRules :: Rules ()
myModelRules = do
    myModelRule ~> need
        [ ruleForReplicationEffects
        ]

    subRules
        myModelRulesFor
        ruleForReplicationEffects
        allReplicationEffectExperiments


ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-my-models"

myModelTexFile :: ExperimentConfig a => a -> FilePath
myModelTexFile ecf = reportsTmpDir </> experimentTarget ecf ++ "-my-model" <.> texExt

myModelFileForReport :: ExperimentConfig a => a -> Int -> FilePath
myModelFileForReport ecf = (myModelTexFile ecf `modelFileForReport`)

useMyModelInReport :: ExperimentConfig a => a -> Int -> Rules ()
useMyModelInReport ecf i = myModelFileForReport ecf i `byCopying` myModelTexFile ecf

dependOnMyModelForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnMyModelForReport ecf i = need [myModelFileForReport ecf i]

myModelRuleFor :: ExperimentConfig a => a -> String
myModelRuleFor ecf = experimentTarget ecf ++ "-my-model"

myModelRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
myModelRulesFor ecf = onlyIfResultsExist ecf $ do
    let rule = myModelRuleFor ecf
    rule ~> need []
    pure rule

readMyModelFile :: MonadIO m => FilePath -> m MyModel
readMyModelFile = readJSON

myEstimationRuleFor :: ExperimentConfig a => a -> String
myEstimationRuleFor ecf = experimentTarget ecf ++ "-my-model-estimation"

myModelEstimateFileFor :: ExperimentConfig a => a -> [FilePath] -> FilePath
myModelEstimateFileFor ecf = changeFilename (const "my-model-estimate") . (`replaceSndDir` experimentAnalysisTmpDir ecf) . head

myEstimationRulesFor :: ExperimentConfig a => a -> Rules String
myEstimationRulesFor ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    myModelFiles <- forM slocss $ \slocs -> do
        let modelFile = myModelEstimateFileFor ecf slocs
        modelFile %> \outf -> do
            myModel <- estimateMyModel ecf slocs
            writeJSON outf myModel
        return modelFile

    let mytarget = myEstimationRuleFor ecf
    mytarget ~> need myModelFiles
    return mytarget

estimateMyModel :: ExperimentConfig a => a -> [FilePath] -> Action MyModel
estimateMyModel ecf slocs = do
    let mm1 = MM1Model 0 0
    let mmm = MMmModel 0 0 0
    pure $ MyModel mm1 mmm mm1 mm1

