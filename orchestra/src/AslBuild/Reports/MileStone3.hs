module AslBuild.Reports.MileStone3 where

import           Development.Shake

import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Models.MM1

import           AslBuild.Reports.Common
import           AslBuild.Reports.MileStone1

report3Rules :: Rules ()
report3Rules = report 3 texPreAction customRules
  where
    customRules =
        remoteStabilityTrace `useMM1ModelInReport` 3
    texPreAction = do
        remoteStabilityTrace `dependOnMM1ModelForReport` 3
        need [architecturePng]
