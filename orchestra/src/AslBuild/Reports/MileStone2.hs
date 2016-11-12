module AslBuild.Reports.MileStone2 where

import           Development.Shake

import           AslBuild.Analysis.MaximumThroughput
import           AslBuild.Analysis.TraceSlice
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Reports.Common

report2Rules :: Rules ()
report2Rules = report 2 texPreAction customRules
  where
    customRules = do
        useThroughputAnalysisPlotsInReport remoteMaximumThroughput 2
        useTraceSlicePlotsInReport remoteMaximumThroughput 2

    texPreAction = do
        dependOnThroughputAnalysisPlotsForReport remoteMaximumThroughput 2
        dependOnTraceSlicePlotsForReport remoteMaximumThroughput 2
