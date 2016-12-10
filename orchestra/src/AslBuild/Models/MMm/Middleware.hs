module AslBuild.Models.MMm.Middleware where

import           Control.Monad.IO.Class

import           Development.Shake.FilePath

import           AslBuild.Analysis.Utils
import           AslBuild.Experiment
import           AslBuild.Utils

import           AslBuild.Models.MM1.Middleware
import           AslBuild.Models.MMm.Types
import           AslBuild.Models.MMm.Utils

mmmMiddlewareModelFileFor :: ExperimentConfig a => a -> FilePath -> FilePath
mmmMiddlewareModelFileFor ecf = changeFilename (++ "-mmm-middleware") . (`replaceDirectory` experimentAnalysisTmpDir ecf)

calcMiddlewareMMmModel :: MonadIO m => FilePath -> m MMmModel
calcMiddlewareMMmModel sloc = do
    mm1 <- calcMiddlewareMM1Model sloc
    fromMM1WithSummaryLocation mm1 sloc
