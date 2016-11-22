module AslBuild
    ( aslBuild
    ) where

import           System.Environment (getArgs, withArgs)

import           AslBuild.Build

aslBuild :: IO ()
aslBuild = do
    args <- getArgs
    let shakeArgs = "--color" : "--jobs" : args -- Always use color and multiple threads for shake
    withArgs shakeArgs doTheShake
