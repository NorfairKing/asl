module AslBuild.Reports.MileStone2 where

import           Development.Shake

import           AslBuild.Reports.Common

report2Rules :: Rules ()
report2Rules = report 2 customRules texPreAction
  where
    customRules = return ()
    texPreAction = return ()
