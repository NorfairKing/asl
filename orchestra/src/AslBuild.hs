module AslBuild
    ( aslBuild
    ) where

import           Data.List.Split
import           System.Environment (getArgs, withArgs)

import           AslBuild.Build
import           AslBuild.Clean
import           AslBuild.OptParse

aslBuild :: IO ()
aslBuild = do
    args <- getArgs
    let splitt = splitOn ["--"] args
    case splitt of
        [] -> putStrLn "There is something wrong in the split library."
        (first:rests) -> do
            let rest = concat rests
            (command, _) <- getInstructions first
            let shakeArgs = "--color" : "--jobs" : rest -- Always use color and multiple threads for shake
                buildTarget target = withArgs (target : shakeArgs) doTheShake
            case command of
                DispatchBuild target -> buildTarget target
                DispatchClean -> buildTarget cleanRule
