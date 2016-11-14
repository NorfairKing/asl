module AslBuild.Reports.MileStone2 where

import           Development.Shake

-- import           AslBuild.Analysis.MaximumThroughput
-- import           AslBuild.Analysis.TraceSlice
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Reports.Common
import           AslBuild.Reports.ExperimentFormat

report2Rules :: Rules ()
report2Rules = report 2 texPreAction customRules
  where
    customRules = do
        -- remoteMaximumThroughput `useThroughputAnalysisPlotsInReport` 2
        -- remoteMaximumThroughput `useTraceSlicePlotsInReport` 2
        remoteMaximumThroughput `useExperimentTableInReport` 2
        remoteReplicationEffect `useExperimentTableInReport` 2
        remoteWriteEffect `useExperimentTableInReport` 2

    texPreAction = do
        -- remoteMaximumThroughput `dependOnThroughputAnalysisPlotsForReport` 2
        -- remoteMaximumThroughput `dependOnTraceSlicePlotsForReport` 2
        remoteMaximumThroughput `dependOnExperimentTableForReport` 2
        remoteReplicationEffect `dependOnExperimentTableForReport` 2
        remoteWriteEffect `dependOnExperimentTableForReport` 2
