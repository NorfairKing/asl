module AslBuild.Reports.ExperimentFormat where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Reports.ExperimentFormat.Types

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
