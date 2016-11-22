{-# LANGUAGE RecordWildCards #-}
module AslBuild.Reports.ExperimentFormat.Types where

import           Data.List

import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput.Types
import           AslBuild.Experiments.ReplicationEffect.Types
import           AslBuild.Experiments.WriteEffect.Types
import           AslBuild.Memaslap
import           AslBuild.Reports.Logfiles
import           AslBuild.Types

class ExperimentFormat a where
    renderSetupTable :: a -> String

-- TODO separate info on seperate lines a little better
instance ExperimentFormat MaximumThroughputCfg where
    renderSetupTable ecf@MaximumThroughputCfg{..} = tabular $
        [ "Number of server machines & " ++ show (nrServers hlConfig)
        , "Number of client machines & " ++ show (nrClients hlConfig)
        , "Virtual clients per machine & " ++ showMinMaxList (map snd threadConcTups)
        , workloadLine
        , writePercentageLine [0]
        , "Replication & No replication ($R=1$)"
        , "Middleware threads per read pool & " ++ showMinMaxList (nub $ map fst threadConcTups)
        , "Runtime x repetitions & " ++ timeUnit mtRuntime ++ " x 1"
        ] ++ logfileLines ecf

instance ExperimentFormat ReplicationEffectCfg where
    renderSetupTable ecf@ ReplicationEffectCfg{..} = tabular $
        [ "Number of server machines & " ++ showIntList serverCounts
        , "Number of client machines & " ++ show (nrClients hlConfig)
        , "Virtual clients per machine & " ++ show defaultConcurrency
        , workloadLine
        , writePercentageLine [0.05]
        , "Replication & " ++ showMathList ["1", "\\lceil S/2 \\rceil", "S"]
        , "Middleware threads per read pool & " ++ show defaultMiddleThreads
        , "Runtime x repetitions & " ++ timeUnit reRuntime ++ " x 1"
        ] ++ logfileLines ecf

instance ExperimentFormat WriteEffectCfg where
    renderSetupTable ecf@WriteEffectCfg{..} = tabular $
        [ "Number of server machines & " ++ showIntList serverCounts
        , "Number of client machines & " ++ show (nrClients hlConfig)
        , "Virtual clients per machine & " ++ show defaultConcurrency
        , workloadLine
        , writePercentageLine writePercentages
        , "Replication & " ++ showMathList ["1", "S"]
        , "Middleware threads per read pool & " ++ show defaultMiddleThreads
        , "Runtime x repetitions & " ++ timeUnit weRuntime ++ " x 1"
        ] ++ logfileLines ecf

workloadLine :: String
workloadLine = workloadLineFor defaultMemaslapConfig

workloadLineFor :: MemaslapConfig -> String
workloadLineFor MemaslapConfig{..} =
    "Workload & " ++
    intercalate ", "
        [ "Key " ++ renderDistrList keysizeDistributions ++ "B"
        , "Value " ++ renderDistrList valueDistributions ++ "B"
        ]
  where
    renderDistrList [d] = renderDistr d
    renderDistrList _ = error "not implemented yet."
    renderDistr Distribution{..} =
        if distrMin == distrMax
        then show distrMin
        else show distrMin ++ "-" ++ show distrMax

writePercentageLine :: [Double] -> String
writePercentageLine pers = "Write percentage & " ++ showPercentageList pers

logfileLines :: ExperimentConfig a => a -> [String]
logfileLines ecf =
    [ "Log files & " ++ experimentClientLogsShort ecf
    , " & " ++ experimentMiddleTracesShort ecf
    ]

showMinMaxList :: [Int] -> String
showMinMaxList = showMinMaxListWith show

showMinMaxListWith :: Ord a => (a -> String) -> [a] -> String
showMinMaxListWith func [l] = func l
showMinMaxListWith func ls = "[" ++ func (minimum ls) ++ " .. " ++ func (maximum ls) ++ "]"

tabular :: [String] -> String
tabular rows = unlines $
    [ "\\begin{tabular}{|c|c|}" ]
    ++ map tabRow rows ++
    [ "\\hline"
    , "\\end{tabular}"
    ]

tabRow :: String -> String
tabRow row = "\\hline " ++ row ++ "\\\\"

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
showListWith func ds = "[" ++ intercalate ", " (map func ds) ++ "]"
