module AslBuild
    ( aslBuild
    ) where

import           Data.List.Split
import           System.Environment (getArgs, withArgs)

import           AslBuild.Build
import           AslBuild.Create
import           AslBuild.OptParse
import           AslBuild.Run

aslBuild :: IO ()
aslBuild = do
    args <- getArgs
    let splitt = splitOn ["--"] args
    case splitt of
        [] -> putStrLn "There is something wrong in the split library."
        (first:rests) -> do
            let rest = concat rests
            (command, _) <- getInstructions first
            withArgs rest $
                case command of
                    DispatchBuild buildCtx -> doTheShake buildCtx
                    DispatchTest -> doTheShake BuildTest
                    DispatchRun runCtx  -> run runCtx
                    DispatchCreate createCtx -> create createCtx
