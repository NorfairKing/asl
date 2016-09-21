module AslBuild.Run where

import           AslBuild.OptParse
import           AslBuild.RunBaseLine

run :: RunContext -> IO ()
run ctx = case ctx of
    RunBaseLine blc -> runBaseLine blc
