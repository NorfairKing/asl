module AslBuild.Reports where

import Development.Shake

import AslBuild.Reports.Common
import AslBuild.Reports.ExperimentFormat
import AslBuild.Reports.Logfiles
import AslBuild.Reports.MileStone1
import AslBuild.Reports.MileStone2
import AslBuild.Reports.MileStone3
import AslBuild.Reports.SignTable

reportsRule :: String
reportsRule = "reports"

cleanReportsRule :: String
cleanReportsRule = "clean-reports"

reportRules :: Rules ()
reportRules = do
    let reportNrs = [2, 3]
    reportsRule ~> need (map reportRule reportNrs)
    phony cleanReportsRule $ need $ map reportCleanRule reportNrs
    experimentTablesRules
    logfileListingRules
    signTableRules
    report1Rules
    report2Rules
    report3Rules
