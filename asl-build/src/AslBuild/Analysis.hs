module AslBuild.Analysis where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad.Reader

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.RunBaseLine

import           AslBuild.Analysis.BuildR

analysisRules :: AslBuilder ()
analysisRules = do
    c <- ask
    lift $ do
        let analysisTargets = localhostPlots
        case c of
            BuildAll _ -> want analysisTargets
            BuildAnalysis -> want analysisTargets
            _ -> return ()

        analysis
    rBuildRules

localResults :: FilePath
localResults = csvOut

analysisDir :: FilePath
analysisDir = "analysis"

baselineLocalhostPrefix :: FilePath
baselineLocalhostPrefix = "baseline-localhost"

localhostPlotTps :: FilePath
localhostPlotTps = analysisDir </> (baselineLocalhostPrefix ++ "-tps") <.> pngExt

localhostPlotAvg :: FilePath
localhostPlotAvg = analysisDir </> (baselineLocalhostPrefix ++ "-avg") <.> pngExt

localhostPlots :: [FilePath]
localhostPlots = [localhostPlotTps, localhostPlotAvg]

analysisScript :: FilePath
analysisScript = analysisDir </> "analyze.r"

analysis :: Rules ()
analysis =
    localhostPlots &%> \_ -> do
        need [rScriptBin, analysisScript]--, localResults]
        cmd rScriptBin analysisScript localResults localhostPlotTps localhostPlotAvg


