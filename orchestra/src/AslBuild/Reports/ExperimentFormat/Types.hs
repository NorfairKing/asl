{-# LANGUAGE RecordWildCards #-}
module AslBuild.Reports.ExperimentFormat.Types where

import           Data.List

import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput.Types
import           AslBuild.Experiments.ReplicationEffect.Types
import           AslBuild.Experiments.WriteEffect.Types
import           AslBuild.Types

class ExperimentFormat a where
    renderSetupTable :: a -> String

-- TODO separate info on seperate lines a little better
instance ExperimentFormat MaximumThroughputCfg where
    renderSetupTable MaximumThroughputCfg{..} = tabular
        [ "Number of server machines & " ++ show (nrServers hlConfig)
        , "Number of client machines & " ++ show (nrClients hlConfig)
        , "Virtual clients per machine & " ++ show (nub $ map snd threadConcTups)
        , workloadLine
        , writePercentageLine [0]
        , "Replication & No replication ($R=1$)"
        , "Middleware threads per read pool & " ++ show (nub $ map fst threadConcTups)
        , "Runtime x repetitions & " ++ timeUnit mtRuntime ++ " x 1"
        , "Log files & " ++ "TODO"
        ]

instance ExperimentFormat ReplicationEffectCfg where
    renderSetupTable ReplicationEffectCfg{..} = tabular
        [ "Number of server machines & " ++ show serverCounts
        , "Number of client machines & " ++ show (nrClients hlConfig)
        , "Virtual clients per machine & " ++ show defaultConcurrency
        , workloadLine
        , writePercentageLine [0.05]
        , "Replication & [$1$, $\\lceil S/2 \\rceil$, $S$]"
        , "Middleware threads per read pool & " ++ show defaultMiddleThreads
        , "Runtime x repetitions & " ++ timeUnit reRuntime ++ " x 1"
        , "Log files & " ++ "TODO"
        ]

instance ExperimentFormat WriteEffectCfg where
    renderSetupTable WriteEffectCfg{..} = tabular
        [ "Number of server machines & " ++ show serverCounts
        , "Number of client machines & " ++ show (nrClients hlConfig)
        , "Virtual clients per machine & " ++ show defaultConcurrency
        , workloadLine
        , writePercentageLine writePercentages
        , "Replication & [$1$, $S$]"
        , "Middleware threads per read pool & " ++ show defaultMiddleThreads
        , "Runtime x repetitions & " ++ timeUnit weRuntime ++ " x 1"
        , "Log files & " ++ "TODO"
        ]

workloadLine :: String
workloadLine = "Workload & Key 16B, Value 128B"

writePercentageLine :: [Double] -> String
writePercentageLine pers = "Write percentage & " ++ render pers
  where
    render [d] = showPer d ++ "\\%"
    render ds = ("[" ++) . (++ "]") . intercalate ", " $ map ((\d -> "$" ++ d ++ "$\\%") . showPer) ds
    showPer = show . (floor :: Double -> Int) . (* 100)

tabular :: [String] -> String
tabular rows = unlines $
    [ "\\begin{tabular}{|c|c|}" ]
    ++ map tabRow rows ++
    [ "\\hline"
    , "\\end{tabular}"
    ]

tabRow :: String -> String
tabRow row = "\\hline " ++ row ++ "\\\\"
