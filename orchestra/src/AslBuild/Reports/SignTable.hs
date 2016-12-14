{-# LANGUAGE RecordWildCards #-}
module AslBuild.Reports.SignTable where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.Factorial
import           AslBuild.Experiments.Factorial.Types
import           AslBuild.Reports.Common
import           AslBuild.Utils


signTableRule :: String
signTableRule = "sign-table"

signTableRules :: Rules ()
signTableRules = do
    rs <- mapM signTableRulesFor allFactorialExperiments
    signTableRule ~> need rs

signTableRuleFor :: ExperimentConfig a => a -> String
signTableRuleFor ecf = experimentTarget ecf ++ "-sign-table"

signTableRulesFor :: FactorialCfg -> Rules String
signTableRulesFor ecf = do
    let signTableF = signTableFile ecf
    signTableF %> \_ -> do
        sTable <- genSignTable ecf
        writeFile' signTableF sTable

    let thisTarget = signTableRuleFor ecf
    thisTarget ~> need [signTableF]
    return thisTarget

signTableFile :: ExperimentConfig a => a -> FilePath
signTableFile ecf = reportsTmpDir </> experimentTarget ecf ++ "-sign-table" <.> texExt

signTableFileForReport :: FilePath -> Int -> FilePath
signTableFileForReport file i = file `replaceDirectory` reportGenfileDir i

useSignTableInReport :: ExperimentConfig a => a -> Int -> Rules ()
useSignTableInReport ecf i = signTableFileForReport eff i `byCopying` eff
  where eff = signTableFile ecf

dependOnSignTableForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnSignTableForReport ecf i = need [signTableFileForReport (signTableFile ecf) i]

genSignTable :: FactorialCfg -> Action String
genSignTable FactorialCfg{..} =
    pure "hi"
