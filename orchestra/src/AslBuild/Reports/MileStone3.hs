module AslBuild.Reports.MileStone3 where

import Development.Shake

import AslBuild.Experiments.Extreme
import AslBuild.Experiments.Factorial
import AslBuild.Experiments.ReplicationEffect
import AslBuild.Experiments.StabilityTrace
import AslBuild.Experiments.ThinkTime
import AslBuild.IRTL
import AslBuild.Models.MM1
import AslBuild.Models.MMm
import AslBuild.Models.MyModel

import AslBuild.Reports.Common
import AslBuild.Reports.ExperimentFormat
import AslBuild.Reports.Logfiles
import AslBuild.Reports.MileStone1
import AslBuild.Reports.SignTable

report3Rules :: Rules ()
report3Rules = report 3 texPreAction customRules
  where
    customRules = do
        remoteStabilityTrace `useMM1ModelInReport` 3
        remoteReplicationEffect `useMMmModelInReport` 3
        remoteReplicationEffect `useMMmPlotsInReport` 3
        remoteReplicationEffect `useIrtlGenfileInReport` 3
        remoteReplicationEffect `useIrtlPlotInReport` 3
        remoteReplicationEffect `useIrtlThinkTimeFileInReport` 3
        remoteReplicationEffect `useExperimentTableInReport` 3
        remoteStabilityTrace `useExperimentTableInReport` 3
        remoteThinkTime `useExperimentTableInReport` 3
        remoteFactorial `useSignTableInReport` 3
        remoteFactorial `useExperimentTableInReport` 3
        remoteExtreme `useExperimentTableInReport` 3
        remoteExtreme `useMyModelInReport` 3
        useTheseLogfileListingsForReport
            3
            [ loglistingFileFor remoteStabilityTrace
            , loglistingFileFor remoteThinkTime
            , loglistingFileFor remoteReplicationEffect
            , loglistingFileFor remoteFactorial
            , loglistingFileFor remoteExtreme
            ]
    texPreAction = do
        remoteStabilityTrace `dependOnMM1ModelForReport` 3
        remoteReplicationEffect `dependOnMMmModelForReport` 3
        remoteReplicationEffect `dependOnMMmPlotsForReport` 3
        remoteReplicationEffect `dependOnIrtlGenfileForReport` 3
        remoteReplicationEffect `dependOnIrtlPlotForReport` 3
        remoteReplicationEffect `dependOnIrtlThinkTimeFileForReport` 3
        remoteReplicationEffect `dependOnExperimentTableForReport` 3
        remoteStabilityTrace `dependOnExperimentTableForReport` 3
        remoteThinkTime `dependOnExperimentTableForReport` 3
        remoteFactorial `dependOnSignTableForReport` 3
        remoteFactorial `dependOnExperimentTableForReport` 3
        remoteExtreme `dependOnExperimentTableForReport` 3
        remoteExtreme `dependOnMyModelForReport` 3
        need [architecturePng]
        dependOnLogfileListingsForReport 3
