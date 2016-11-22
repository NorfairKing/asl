module AslBuild.Reports.Logfiles where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Reports.Common
import           AslBuild.Utils

logfileListingRule :: String
logfileListingRule = "logfile-listings"

logfileListingRules :: Rules ()
logfileListingRules = do
    ls1 <- mapM logfileListingRulesFor allMaximumThroughputExperiments
    ls2 <- mapM logfileListingRulesFor allReplicationEffectExperiments
    ls3 <- mapM logfileListingRulesFor allWriteEffectExperiments

    logfileListingRule ~> need (ls1 ++ ls2 ++ ls3)

logfileListingRuleFor :: ExperimentConfig a => a -> String
logfileListingRuleFor ecf = experimentTarget ecf ++ "-logfile-listings"

logfileListingRulesFor :: ExperimentConfig a => a -> Rules String
logfileListingRulesFor ecf = do
    let llFile = loglistingFileFor ecf
    llFile %> \_ -> writeFile' llFile $ genLogfileListingFor ecf

    let thisTarget = logfileListingRuleFor ecf
    thisTarget ~> need [llFile]
    return thisTarget

loglistingFileFor :: ExperimentConfig a => a -> FilePath
loglistingFileFor ecf = reportsTmpDir </> experimentTarget ecf ++ "-logfile-listing" <.> texExt

genLogfileListingFor :: ExperimentConfig a => a -> String
genLogfileListingFor ecf = unlines
    $ map (inHline . uncurry (\a b -> a ++ " & {\\footnotesize \\url{https://gitlab.inf.ethz.ch/" ++ myNetzh ++ "/asl-fall16-project/blob/master/results/" ++ b ++ "}}"))
    [ (experimentClientLogsShort ecf, experimentClientLogsLong ecf)
    , (experimentMiddleTracesShort ecf, experimentMiddleTracesLong ecf)
    ]

inHline :: String -> String
inHline s = "\\hline " ++ s ++ "\\\\"

experimentClientLogsShort :: ExperimentConfig a => a -> String
experimentClientLogsShort ecf = dropRemotePrefix (experimentTarget ecf) ++ "-client-logs"

experimentClientLogsLong :: ExperimentConfig a => a -> String
experimentClientLogsLong ecf = localClientLogDir ecf ++ "/*"

experimentMiddleTracesShort :: ExperimentConfig a => a -> String
experimentMiddleTracesShort ecf = dropRemotePrefix (experimentTarget ecf) ++ "-middle-traces"

dropRemotePrefix :: String -> String
dropRemotePrefix = drop (length "remote-")

experimentMiddleTracesLong :: ExperimentConfig a => a -> String
experimentMiddleTracesLong ecf = localMiddleTraceDir ecf ++ "/*" <.> csvExt

logfileListingFileForReportName :: Int -> String
logfileListingFileForReportName i = "report-" ++ show i ++ "-logfile-listings" <.> texExt

logfileListingFileForReport :: Int -> FilePath
logfileListingFileForReport i = reportsTmpDir </> logfileListingFileForReportName i

logfileListingFileForReportOutfile :: Int -> FilePath
logfileListingFileForReportOutfile i = reportGenfileDir i </> logfileListingFileForReportName i

useTheseLogfileListingsForReport :: Int -> [FilePath] -> Rules ()
useTheseLogfileListingsForReport i fs = do
    let llfile = logfileListingFileForReport i
    llfile %> \_ -> do
        cs <- forP fs readFile'
        writeFile' llfile $ makeCompleteListingWithLines cs

    logfileListingFileForReportOutfile i `byCopying` llfile


dependOnLogfileListingsForReport :: Int -> Action ()
dependOnLogfileListingsForReport i = need [logfileListingFileForReportOutfile i]

makeCompleteListingWithLines :: [String] -> String
makeCompleteListingWithLines cs = unlines $
    [ "\\begin{tabular}{c|p{8cm}}"
    , "\\textbf{Short name }& \\textbf{Location} \\\\"
    ] ++ cs ++
    [ "\\end{tabular}"
    ]




