{-# LANGUAGE RecordWildCards #-}
module AslBuild.Reports.ExperimentFormat.Types where

import           Data.List

import           AslBuild.Experiment.Types
import           AslBuild.Experiments.MaximumThroughput.Types
import           AslBuild.Types

class ExperimentFormat a where
    renderSetupTable :: a -> String

instance ExperimentFormat MaximumThroughputCfg where
    renderSetupTable MaximumThroughputCfg{..} = unlines
        [ "\\begin{tabular}{|c|c|}"
        , "\\hline Number of server machines & " ++ show (nrServers hlConfig) ++ "\\\\"
        , "\\hline Number of client machines & " ++ show (nrClients hlConfig) ++ "\\\\"
        , "\\hline Virtual clients per machine & " ++ show (nub $ map snd threadConcTups) ++ "\\\\"
        , "\\hline Workload & key 16B, Value 128B, Writes 0\\%\\\\"
        , "\\hline Replication & No replication ($R=1$)\\\\"
        , "\\hline Middleware threads & " ++ show (nub $ map fst threadConcTups) ++ "\\\\"
        , "\\hline Runtime x repetitions & " ++ timeUnit mtRuntime ++ "x1" ++ "\\\\"
        , "\\hline Log files & " ++ "TODO" ++ "\\\\"
        , "\\hline"
        , "\\end{tabular}"
        ]
