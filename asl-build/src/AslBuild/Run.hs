module AslBuild.Run where

import           AslBuild.Build
import           AslBuild.OptParse

run :: Experiment -> IO ()
run ex = doTheShake $ BuildRunExperiment ex
