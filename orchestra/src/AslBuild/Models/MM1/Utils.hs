module AslBuild.Models.MM1.Utils where

import           AslBuild.Analysis.Types

import           AslBuild.Models.MM1.Types

mm1TraficIntensity :: MM1Model -> Double
mm1TraficIntensity mm1 = avg (arrivalRate mm1) / avg (serviceRate mm1)

