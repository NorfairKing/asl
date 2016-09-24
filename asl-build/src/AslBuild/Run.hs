module AslBuild.Run where

import           AslBuild.OptParse
import           AslBuild.RunBaseLine
import           AslBuild.RunLocalExperiment

run :: RunContext -> IO ()
run ctx = case ctx of
    RunBaseLine blc -> runBaseLine blc
    RunLocally -> runLocalExperiment
