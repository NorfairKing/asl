module AslBuild.Reports.MileStone2 where

import           Development.Shake

-- import           AslBuild.Analysis.MaximumThroughput
-- import           AslBuild.Analysis.TraceSlice
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Reports.Common
import           AslBuild.Reports.ExperimentFormat

report2Rules :: Rules ()
report2Rules = report 2 texPreAction customRules
  where
    customRules =
        -- remoteMaximumThroughput `useThroughputAnalysisPlotsInReport` 2
        -- remoteMaximumThroughput `useTraceSlicePlotsInReport` 2
        remoteMaximumThroughput `useExperimentTableInReport` 2

    texPreAction =
        -- remoteMaximumThroughput `dependOnThroughputAnalysisPlotsForReport` 2
        -- remoteMaximumThroughput `dependOnTraceSlicePlotsForReport` 2
        remoteMaximumThroughput `dependOnExperimentTableForReport` 2
