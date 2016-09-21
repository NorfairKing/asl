module AslBuild
    ( aslBuild
    ) where

import           Data.List.Split
import           System.Environment (getArgs, withArgs)

import           AslBuild.Build
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
            (command, _) <- getArguments first
            withArgs rest $
                case command of
                    CommandBuild buildCtx -> doTheShake buildCtx
                    CommandRun runCtx  -> run runCtx
