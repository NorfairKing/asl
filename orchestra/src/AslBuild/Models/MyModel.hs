module AslBuild.Models.MyModel where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Client.Types
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Memaslap.Types
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Models.MM1.Types
import           AslBuild.Models.MMInf.Types
import           AslBuild.Models.MMm.Types
import           AslBuild.Models.MyModel.Types
import           AslBuild.Models.Utils
import           AslBuild.Utils

myModelRule :: String
myModelRule = "my-models"

myModelRules :: Rules ()
myModelRules = do
    myModelRule ~> need
        [ ruleForReplicationEffects
        ]

    subRules
        myModelRulesFor
        ruleForReplicationEffects
        allReplicationEffectExperiments


ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-my-models"

myModelTexFile :: ExperimentConfig a => a -> FilePath
myModelTexFile ecf = reportsTmpDir </> experimentTarget ecf ++ "-my-model" <.> texExt

myModelFileForReport :: ExperimentConfig a => a -> Int -> FilePath
myModelFileForReport ecf = (myModelTexFile ecf `modelFileForReport`)

useMyModelInReport :: ExperimentConfig a => a -> Int -> Rules ()
useMyModelInReport ecf i = myModelFileForReport ecf i `byCopying` myModelTexFile ecf

dependOnMyModelForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnMyModelForReport ecf i = need [myModelFileForReport ecf i]

myModelRuleFor :: ExperimentConfig a => a -> String
myModelRuleFor ecf = experimentTarget ecf ++ "-my-model"

myModelRulesFor :: ExperimentConfig a => a -> Rules (Maybe String)
myModelRulesFor ecf = onlyIfResultsExist ecf $ do
    estr <- myEstimationRulesFor ecf

    let rule = myModelRuleFor ecf
    rule ~> need [estr]
    pure rule

readMyModelFile :: MonadIO m => FilePath -> m MyModel
readMyModelFile = readJSON

myEstimationRuleFor :: ExperimentConfig a => a -> String
myEstimationRuleFor ecf = experimentTarget ecf ++ "-my-model-estimation"

myModelEstimateFileFor :: ExperimentConfig a => a -> [FilePath] -> FilePath
myModelEstimateFileFor ecf = changeFilename (const "my-model-estimate") . (`replaceSndDir` experimentAnalysisTmpDir ecf) . head

myEstimationRulesFor :: ExperimentConfig a => a -> Rules String
myEstimationRulesFor ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    myModelFiles <- forM slocss $ \slocs -> do
        let modelFile = myModelEstimateFileFor ecf slocs
        modelFile %> \outf -> do
            myModel <- estimateMyModel ecf slocs
            writeJSON outf myModel
        return modelFile

    let mytarget = myEstimationRuleFor ecf
    mytarget ~> need myModelFiles
    return mytarget

estimateMyModel :: ExperimentConfig a => a -> [FilePath] -> Action MyModel
estimateMyModel ecf slocs = do
    ers <- mapM readResultsSummary slocs
    mrfs <- case mapM merMiddleResultsFile ers of
        Nothing  -> fail "Need middles for my model."
        Just mes -> pure mes

    let combinedResultsFile = combinedClientRepsetResultsFile ecf slocs
    let combinedAvgDursFile = combinedAvgDurationFile ecf mrfs
    let combinedAvgReadDursFile = combinedAvgReadDurationFile ecf mrfs
    let combinedAvgWriteDursFile = combinedAvgWriteDurationFile ecf mrfs
    let neededFiles = [combinedResultsFile, combinedAvgDursFile, combinedAvgReadDursFile, combinedAvgWriteDursFile]
    need neededFiles
    putLoud $ unwords ["Making my model from:", show neededFiles]

    setup <- readExperimentSetupForSummary $ head ers
    cres <- readCombinedClientsResults combinedResultsFile
    avgDurs <- readCombinedAvgDursFile combinedAvgDursFile
    avgReadDurs <- readCombinedAvgDursFile combinedAvgReadDursFile
    avgWriteDurs <- readCombinedAvgDursFile combinedAvgWriteDursFile

    (middleSetup, serverSetups) <- case backendSetup setup of
        Left _    -> fail "need middlesetup for my model."
        Right tup -> pure tup

    let unMiddleTime = (/ (10 ** 9))
        -- unMemaTime = (/ (10 ** 6))

    let parsingTime = avgAvgs (untilParsedTime avgDurs)
        enqueuingTime = avgAvgs (untilEnqueuedTime avgDurs)

    let nrSers = genericLength serverSetups
        setProp = setProportion $ msConfig $ cMemaslapSettings $ head $ clientSetups setup
        getProp = 1 - setProp
        nrReadThreads = mwNrThreads $ mMiddlewareFlags middleSetup

    let readServiceTime = unMiddleTime $
              avgAvgs (untilAskedTime avgReadDurs)
            + avgAvgs (untilRepliedTime avgReadDurs)
            + avgAvgs (untilRespondedTime avgReadDurs)

    let writer1ServiceTime = unMiddleTime $ avgAvgs (untilAskedTime avgWriteDurs)
    let writer2ServiceTime = unMiddleTime $ avgAvgs (untilRepliedTime avgWriteDurs)

    -- Arrival rate at acceptor = average throughput
    let accλ = avgAvgs $ avgBothResults $ avgTpsResults cres
    -- Service time at acceptor = average parsing + hashing + enqueueing
    -- Service rate is inverse of that.
    let accμ = (1/) $ unMiddleTime $ parsingTime + enqueuingTime

    let serverWorkerλ = accλ / nrSers

    -- Arrival rate at get workers
    let getλ = getProp * serverWorkerλ
    let getμ = 1 / readServiceTime
    let getm = nrReadThreads

    -- Arrival rate at set workers
    let setλ = setProp * serverWorkerλ
    let setμ = 1 / writer1ServiceTime

    -- Arrival rate at set worker infs
    let setIλ = setλ
    let setIμ = 1 / writer2ServiceTime

    let accModel_ = MM1Model accλ accμ
        getModel_ = MMmModel getλ getμ getm
        setModel_ = MM1Model setλ setμ
        setModelInf_ = MMInfModel setIλ setIμ

    pure $ MyModel accModel_ getModel_ setModel_ setModelInf_

