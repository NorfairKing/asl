module AslBuild.Reports where

import           Development.Shake

import           AslBuild.Reports.Common
import           AslBuild.Reports.MileStone1
import           AslBuild.Reports.MileStone2

reportsRule :: String
reportsRule = "reports"

cleanReportsRule :: String
cleanReportsRule = "clean-reports"

reportRules :: Rules ()
reportRules = do
    let reportNrs = [1..2]
    reportsRule ~> need (map reportRule reportNrs)

    phony cleanReportsRule $ need $ map reportCleanRule reportNrs

    report1Rules
    report2Rules

