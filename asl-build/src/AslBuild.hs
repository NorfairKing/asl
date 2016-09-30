module AslBuild
    ( aslBuild
    ) where

import           Data.List.Split
import           System.Environment    (getArgs, withArgs)

import           AslBuild.Baseline
import           AslBuild.Build
import           AslBuild.Clean
import           AslBuild.LocalLogTest
import           AslBuild.OptParse
import           AslBuild.Test

aslBuild :: IO ()
aslBuild = do
    args <- getArgs
    let splitt = splitOn ["--"] args
    case splitt of
        [] -> putStrLn "There is something wrong in the split library."
        (first:rests) -> do
            let rest = concat rests
            (command, _) <- getInstructions first
            let shakeArgs = "--color" : rest -- Always use color for shake
                buildTarget target = withArgs (target : shakeArgs) doTheShake
            case command of
                DispatchBuild target -> buildTarget target
                DispatchClean -> buildTarget cleanRule
                DispatchTest -> buildTarget testRule
                DispatchRun experiment -> buildTarget $ case experiment of
                    LocalLogTestExperiment -> localLogTestRule
                    LocalBaselineExperiment -> localBaselineExperimentRule
                    RemoteBaselineExperiment -> remoteBaselineExperimentRule
