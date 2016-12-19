module AslBuild.Models.Utils where

import           Development.Shake.FilePath

import           AslBuild.Reports.Common

modelFileForReport :: FilePath -> Int -> FilePath
modelFileForReport file i = file `replaceDirectory` (reportGenfileDir i </> "models")

modelDirForReport :: Int -> FilePath
modelDirForReport i = reportGenfileDir i </> "models"

