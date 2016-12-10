module AslBuild.Reports.MileStone3 where

import           Development.Shake

import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Models.MM1

import           AslBuild.Reports.Common

report3Rules :: Rules ()
report3Rules = report 3 texPreAction customRules
  where
    customRules =
        remoteStabilityTrace `useMM1ModelInReport` 3
    texPreAction =
        remoteStabilityTrace `dependOnMM1ModelForReport` 3
