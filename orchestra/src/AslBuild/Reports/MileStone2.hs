module AslBuild.Reports.MileStone2 where

import           Development.Shake


import           AslBuild.Analysis.MaximumThroughput
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Reports.Common

report2Rules :: Rules ()
report2Rules = report 2 texPreAction customRules
  where
    customRules =
        useMaximumThroughputPlotsInReport localMaximumThroughput 2
    texPreAction =
        dependOnMaximumThroughputPlotsForReport localMaximumThroughput 2
