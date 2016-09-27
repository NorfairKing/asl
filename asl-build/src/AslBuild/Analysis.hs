module AslBuild.Analysis where

import           Development.Shake
import           Development.Shake.FilePath

import           Control.Monad.Reader

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.RunBaseLine

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

rCmd :: FilePath
rCmd = "Rscript"

analysis :: Rules ()
analysis =

    localhostPlots &%> \_ -> do
        need [analysisScript]--, localResults]
        cmd rCmd analysisScript localResults localhostPlotTps localhostPlotAvg

