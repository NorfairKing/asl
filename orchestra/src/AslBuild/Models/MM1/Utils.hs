module AslBuild.Models.MM1.Utils where

import           AslBuild.Analysis.Types

import           AslBuild.Models.MM1.Types

mm1TrafficIntensity :: MM1Model -> Double
mm1TrafficIntensity mm1 = λ / μ
  where
    λ = avg (arrivalRate mm1)
    μ = avg (serviceRate mm1)

mm1MeanResponseTime :: MM1Model -> Double
mm1MeanResponseTime mm1 = (1 / μ) / (1 - ρ)
  where
    ρ = mm1TrafficIntensity mm1
    μ = avg (serviceRate mm1)

mm1StdDevResponseTime :: MM1Model -> Double
mm1StdDevResponseTime mm1 = sqrt $ (1 / (μ ** 2)) / ((1 - ρ) ** 2)
  where
    ρ = mm1TrafficIntensity mm1
    μ = avg (serviceRate mm1)

mm1MeanWaitingTime :: MM1Model -> Double
mm1MeanWaitingTime mm1 = ρ * ((1 / μ) / (1 - ρ))
  where
    ρ = mm1TrafficIntensity mm1
    μ = avg (serviceRate mm1)

mm1StdDevWaitingTime :: MM1Model -> Double
mm1StdDevWaitingTime mm1 = sqrt $ ((2 - ρ) * ρ) / ((μ ** 2) * ((1 - ρ) ** 2))
  where
    ρ = mm1TrafficIntensity mm1
    μ = avg (serviceRate mm1)

mm1MeanNrJobs :: MM1Model -> Double
mm1MeanNrJobs mm1 = ρ / (1 - ρ)
  where
    ρ = mm1TrafficIntensity mm1

mm1StdDevNrJobs :: MM1Model -> Double
mm1StdDevNrJobs mm1 = sqrt $ ρ / ((1 - ρ) ** 2)
  where
    ρ = mm1TrafficIntensity mm1
