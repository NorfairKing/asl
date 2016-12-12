module AslBuild.Reports.MileStone3 where

import           Development.Shake

import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.WriteEffect
import           AslBuild.IRTL
import           AslBuild.Models.MM1

import           AslBuild.Reports.Common
import           AslBuild.Reports.MileStone1

report3Rules :: Rules ()
report3Rules = report 3 texPreAction customRules
  where
    customRules = do
        remoteStabilityTrace `useMM1ModelInReport` 3
        remoteWriteEffect `useIrtlGenfileInReport` 3
    texPreAction = do
        remoteStabilityTrace `dependOnMM1ModelForReport` 3
        remoteWriteEffect `dependOnIrtlGenfileForReport` 3
        need [architecturePng]
