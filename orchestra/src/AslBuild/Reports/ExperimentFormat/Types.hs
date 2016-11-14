{-# LANGUAGE RecordWildCards #-}
module AslBuild.Reports.ExperimentFormat.Types where

import           AslBuild.Experiments.MaximumThroughput.Types

class ExperimentFormat a where
    renderSetupTable :: a -> String

instance ExperimentFormat MaximumThroughputCfg where
    renderSetupTable MaximumThroughputCfg{..} = unlines
        [ "\\begin{tabular}"
        , "\\end{tabular}"
        ]
