module AslBuild.Reports.MileStone2 where

import           Development.Shake

import           AslBuild.Analysis.MaximumThroughput
import           AslBuild.Analysis.ReplicationEffect
import           AslBuild.Analysis.TraceSlice
import           AslBuild.Analysis.WriteEffect
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Reports.Common
import           AslBuild.Reports.ExperimentFormat

report2Rules :: Rules ()
report2Rules = report 2 texPreAction customRules
  where
    customRules = do
        localMaximumThroughput `useThroughputAnalysisPlotsInReport` 2
        localMaximumThroughput `useTraceSlicePlotsInReport` 2
        bigLocalMaximumThroughput `useThroughputAnalysisPlotsInReport` 2
        bigLocalMaximumThroughput `useTraceSlicePlotsInReport` 2
        localReplicationEffect `useReplicationEffectPlotsInReport` 2
        localWriteEffect `useWriteEffectPlotsInReport` 2
        remoteMaximumThroughput `useExperimentTableInReport` 2
        remoteReplicationEffect `useExperimentTableInReport` 2
        remoteWriteEffect `useExperimentTableInReport` 2

    texPreAction = do
        localMaximumThroughput `dependOnThroughputAnalysisPlotsForReport` 2
        localMaximumThroughput `dependOnTraceSlicePlotsForReport` 2
        bigLocalMaximumThroughput `dependOnThroughputAnalysisPlotsForReport` 2
        bigLocalMaximumThroughput `dependOnTraceSlicePlotsForReport` 2
        localReplicationEffect `dependOnReplicationEffectPlotsForReport` 2
        localWriteEffect `dependOnWriteEffectPlotsForReport` 2
        remoteMaximumThroughput `dependOnExperimentTableForReport` 2
        remoteReplicationEffect `dependOnExperimentTableForReport` 2
        remoteWriteEffect `dependOnExperimentTableForReport` 2
