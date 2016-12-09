module AslBuild.Reports.MileStone3 where

import           Development.Shake

import           AslBuild.Reports.Common

report3Rules :: Rules ()
report3Rules = report 3 texPreAction customRules
  where
    customRules = pure ()
    texPreAction = pure ()
