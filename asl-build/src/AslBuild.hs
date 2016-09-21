module AslBuild
    ( aslBuild
    ) where

import           Data.List.Split
import           System.Environment (getArgs, withArgs)

import           AslBuild.Build
import           AslBuild.OptParse

aslBuild :: IO ()
aslBuild = do
    args <- getArgs
    let splitt = splitOn ["--"] args
    case splitt of
        [] -> putStrLn "Should never happen"
        (first:rests) -> do
            let rest = concat rests
            (Command, flags) <- getArguments first
            withArgs rest $ doTheShake flags
