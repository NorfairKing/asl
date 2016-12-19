module AslBuild.Models.MMm.Utils where

import Control.Monad.IO.Class

import AslBuild.Experiment

import AslBuild.Models.MM1.Types (MM1Model(..))
import qualified AslBuild.Models.MM1.Types as MM1
import AslBuild.Models.MMm.Types (MMmModel(..))
import qualified AslBuild.Models.MMm.Types as MMm

fromMM1WithSummaryLocation
    :: MonadIO m
    => MM1Model -> FilePath -> m MMmModel
fromMM1WithSummaryLocation mm1 sloc = do
    setup <- readResultsSummary sloc >>= readExperimentSetupForSummary
    let nrs =
            case backendSetup setup of
                Left _ -> 1
                Right (_, ss) -> length ss
    pure $ fromMM1 nrs mm1

fromMM1 :: Int -> MM1Model -> MMmModel
fromMM1 nrs mm1 =
    MMmModel
    { MMm.arrivalRate = MM1.arrivalRate mm1
    , MMm.serviceRate = MM1.serviceRate mm1
    , MMm.nrServers = nrs
    }

mmmTraficIntensity :: MMmModel -> Double
mmmTraficIntensity mmm =
    MMm.arrivalRate mmm / (fromIntegral (MMm.nrServers mmm) * MMm.serviceRate mmm)
