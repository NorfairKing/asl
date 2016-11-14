module AslBuild.Reports.ExperimentFormat where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Reports.Common
import           AslBuild.Reports.ExperimentFormat.Types
import           AslBuild.Utils

experimentTablesRule :: String
experimentTablesRule = "experiment-tables"

experimentTablesRules :: Rules ()
experimentTablesRules = do
    targets <- mapM experimentFormatRulesFor
        [ smallLocalMaximumThroughput
        , localMaximumThroughput
        , smallRemoteMaximumThroughput
        , remoteMaximumThroughput
        ]

    experimentTablesRule ~> need targets

experimentFormatRuleFor :: ExperimentConfig a => a -> String
experimentFormatRuleFor ecf = experimentTarget ecf ++ "-table"

experimentFormatRulesFor :: (ExperimentConfig a, ExperimentFormat a) => a -> Rules String
experimentFormatRulesFor ecf = do
    experimentFormatFile ecf %> \outFile ->
        writeFile' outFile $ renderSetupTable ecf
    let thisRule = experimentFormatRuleFor ecf
    thisRule ~> need [experimentFormatFile ecf]
    return thisRule

experimentFormatFile :: ExperimentConfig a => a -> FilePath
experimentFormatFile ecf = reportsTmpDir </> experimentTarget ecf ++ "-table" <.> texExt

tableFileForReport :: FilePath -> Int -> FilePath
tableFileForReport file i = file `replaceDirectory` reportGenfileDir i

useExperimentTableInReport :: ExperimentConfig a => a -> Int -> Rules ()
useExperimentTableInReport ecf i = tableFileForReport eff i `byCopying` eff
  where eff = experimentFormatFile ecf

dependOnExperimentTableForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnExperimentTableForReport ecf i = need [tableFileForReport (experimentFormatFile ecf) i]

