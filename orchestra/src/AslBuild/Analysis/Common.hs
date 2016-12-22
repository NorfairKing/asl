module AslBuild.Analysis.Common where

import Development.Shake.FilePath

import AslBuild.Constants

commonRLib :: FilePath
commonRLib = analysisDir </> "common.r"
