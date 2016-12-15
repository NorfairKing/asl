{-# LANGUAGE RecordWildCards #-}
module AslBuild.Reports.ExperimentFormat.Types where

import           Data.List

import           AslBuild.Experiment
import           AslBuild.Experiments.Factorial.Types
import           AslBuild.Experiments.MaximumThroughput.Types
import           AslBuild.Experiments.ReplicationEffect.Types
import           AslBuild.Experiments.StabilityTrace.Types
import           AslBuild.Experiments.ThinkTime.Types
import           AslBuild.Experiments.WriteEffect.Types
import           AslBuild.Memaslap
import           AslBuild.Reports.Logfiles
import           AslBuild.Reports.Utils
import           AslBuild.Types

class ExperimentFormat a where
    renderSetupTable :: a -> String

-- TODO separate info on seperate lines a little better
instance ExperimentFormat MaximumThroughputCfg where
    renderSetupTable ecf@MaximumThroughputCfg{..} = tabular $
        [ ["Number of server machines", show (nrServers hlConfig)]
        , ["Number of client machines", show (nrClients hlConfig)]
        , ["Virtual clients per machine", showMinMaxList (map snd threadConcTups)]
        , workloadLine
        , writePercentageLine [0]
        , ["Replication", "No replication ($R=1$)"]
        , ["Middleware threads per read pool", showMinMaxList $ nub $ map fst threadConcTups]
        , runtimeLine ecf mtRuntime
        ] ++ logfileLines ecf

instance ExperimentFormat ReplicationEffectCfg where
    renderSetupTable ecf@ ReplicationEffectCfg{..} = tabular $
        [ ["Number of server machines", showIntList serverCounts]
        , ["Number of client machines", show $ nrClients hlConfig]
        , ["Virtual clients per machine", show defaultConcurrency]
        , workloadLine
        , writePercentageLine [0.05]
        , ["Replication", showMathList ["1", "\\lceil S/2 \\rceil", "S"]]
        , ["Middleware threads per read pool", show defaultMiddleThreads]
        , runtimeLine ecf reRuntime
        ] ++ logfileLines ecf

instance ExperimentFormat WriteEffectCfg where
    renderSetupTable ecf@WriteEffectCfg{..} = tabular $
        [ ["Number of server machines", showIntList serverCounts]
        , ["Number of client machines", show $ nrClients hlConfig]
        , ["Virtual clients per machine", show defaultConcurrency]
        , workloadLine
        , writePercentageLine writePercentages
        , ["Replication", showMathList ["1", "S"]]
        , ["Middleware threads per read pool", show defaultMiddleThreads]
        , runtimeLine ecf weRuntime
        ] ++ logfileLines ecf

instance ExperimentFormat StabilityTraceCfg where
    renderSetupTable ecf@StabilityTraceCfg{..} = tabular $
        [ ["Number of server machines", "3"]
        , ["Number of client machines", "3"]
        , ["Virtual clients per machine", "64"]
        , workloadLine
        , writePercentageLine [0.01]
        , ["Replication", "1"]
        , ["Middleware threads per read pool", show defaultMiddleThreads]
        , runtimeLine ecf runtime
        ] ++ logfileLines ecf

instance ExperimentFormat ThinkTimeCfg where
    renderSetupTable ecf@ThinkTimeCfg{..} = tabular $
        [ ["Number of server machines", "1"]
        , ["Number of client machines", "1"]
        , ["Virtual clients per machine", "1"]
        , workloadLine
        , writePercentageLine [0.01]
        , ["Replication", "1"]
        , ["Middleware threads per read pool", "1"]
        , runtimeLine ecf ttRuntime
        ] ++ logfileLines ecf

instance ExperimentFormat FactorialCfg where
    renderSetupTable ecf@FactorialCfg{..} = tabular
        [ ["Number of server machines", "3"]
        , ["Number of client machines", "3"]
        , ["Virtual clients per machine", show defaultConcurrency]
        , ["Workload "
          , intercalate ", "
            [ "Key " ++ show (distrMin $ head $ keysizeDistributions defaultMemaslapConfig) ++ "B"
            , "Value " ++ showMathList ["128", "1024"] ++ "B"
            ]
          ]
        , writePercentageLine [0.05, 0.5]
        , ["Replication", showMathList ["1", "S"]]
        , ["Middleware threads per read pool", show defaultMiddleThreads]
        , runtimeLine ecf fRuntime
        ]

workloadLine :: [String]
workloadLine = workloadLineFor defaultMemaslapConfig

workloadLineFor :: MemaslapConfig -> [String]
workloadLineFor MemaslapConfig{..} =
    [ "Workload "
    , intercalate ", "
        [ "Key " ++ renderDistrList keysizeDistributions ++ "B"
        , "Value " ++ renderDistrList valueDistributions ++ "B"
        ]
    ]
  where
    renderDistrList [d] = renderDistr d
    renderDistrList _   = error "not implemented yet."
    renderDistr Distribution{..} =
        if distrMin == distrMax
        then show distrMin
        else show distrMin ++ "-" ++ show distrMax

writePercentageLine :: [Double] -> [String]
writePercentageLine pers = ["Write percentage", showPercentageList pers]

logfileLines :: ExperimentConfig a => a -> [[String]]
logfileLines ecf =
    [ ["Log files", experimentClientLogsShort ecf]
    , ["", experimentMiddleTracesShort ecf]
    ]

runtimeLine :: ExperimentConfig a => a -> TimeUnit -> [String]
runtimeLine ecf time =
    [ "Runtime x repetitions"
    , unwords
        [ timeUnit time
        , "x"
        , show $ repititions $ highLevelConfig ecf
        ]
    ]

showMinMaxList :: [Int] -> String
showMinMaxList = showMinMaxListWith show

showMinMaxListWith :: Ord a => (a -> String) -> [a] -> String
showMinMaxListWith func [l] = func l
showMinMaxListWith func ls = "[" ++ func (minimum ls) ++ " .. " ++ func (maximum ls) ++ "]"

showIntList :: [Int] -> String
showIntList = showListWith (showMathNum . show)

showMinMaxPercentageList :: [Double] -> String
showMinMaxPercentageList = showMinMaxListWith showPercentage

showPercentageList :: [Double] -> String
showPercentageList = showListWith showPercentage

showMathList :: [String] -> String
showMathList = showListWith showMathNum

showMathNum :: String -> String
showMathNum d = "$" ++ d ++ "$"

showPercentage :: Double -> String
showPercentage = showMathNum . (++ "\\%") . show . (round :: Double -> Int) . (* 100)

showListWith :: (a -> String) -> [a] -> String
showListWith func [d] = func d
showListWith func ds  = "[" ++ intercalate ", " (map func ds) ++ "]"
