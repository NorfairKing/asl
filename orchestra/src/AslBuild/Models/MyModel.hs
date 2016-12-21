{-# LANGUAGE RecordWildCards #-}

module AslBuild.Models.MyModel where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Hashable
import           Text.Printf

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.BuildR
import           AslBuild.Analysis.Common
import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Trace
import           AslBuild.Analysis.Types
import           AslBuild.Analysis.Utils
import           AslBuild.Client.Types
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.Extreme
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Memaslap.Types
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Models.MyModel.Types
import           AslBuild.Models.Utils
import           AslBuild.Reports.Utils
import           AslBuild.Utils

myModelRule :: String
myModelRule = "my-models"

myModelRules :: Rules ()
myModelRules = do
    myModelRule ~> need [ruleForReplicationEffects, ruleForExtremes]
    subRules myModelRulesFor ruleForReplicationEffects allReplicationEffectExperiments
    subRules myModelRulesFor ruleForExtremes allExtremeExperiments

ruleForReplicationEffects :: String
ruleForReplicationEffects = "replication-effect-my-models"

ruleForExtremes :: String
ruleForExtremes = "extreme-my-models"

myModelRuleFor
    :: ExperimentConfig a
    => a -> String
myModelRuleFor ecf = experimentTarget ecf ++ "-my-model"

myModelRulesFor
    :: ExperimentConfig a
    => a -> Rules (Maybe String)
myModelRulesFor ecf =
    onlyIfResultsExist ecf $ do
        estr <- myEstimationRulesFor ecf
        evtr <- evaluationRulesFor ecf
        tftr <- texfileRulesFor ecf
        let rule = myModelRuleFor ecf
        rule ~> need [estr, evtr, tftr]
        pure rule

readMyModelFile
    :: MonadIO m
    => FilePath -> m MyModel
readMyModelFile = readJSON

myEstimationRuleFor
    :: ExperimentConfig a
    => a -> String
myEstimationRuleFor ecf = experimentTarget ecf ++ "-my-model-estimation"

myModelEstimateFileFor
    :: ExperimentConfig a
    => a -> [FilePath] -> FilePath
myModelEstimateFileFor ecf =
    changeFilename (const "my-model-estimate") .
    (`replaceSndDir` experimentAnalysisTmpDir ecf) . head

myEstimationRulesFor
    :: ExperimentConfig a
    => a -> Rules String
myEstimationRulesFor ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    myModelFiles <-
        forM slocss $ \slocs -> do
            let modelFile = myModelEstimateFileFor ecf slocs
            modelFile %> \outf -> do
                myModel <- estimateMyModel ecf slocs
                writeJSON outf myModel
            return modelFile
    let mytarget = myEstimationRuleFor ecf
    mytarget ~> need myModelFiles
    return mytarget

estimateMyModel
    :: ExperimentConfig a
    => a -> [FilePath] -> Action MyModel
estimateMyModel ecf slocs = do
    ers <- mapM readResultsSummary slocs
    mrfs <-
        case mapM merMiddleResultsFile ers of
            Nothing  -> fail "Need middles for my model."
            Just mes -> pure mes
    let combinedResultsFile = combinedClientRepsetResultsFile ecf slocs
    let combinedAvgDursFile = combinedAvgDurationFile ecf mrfs
    let combinedAvgReadDursFile = combinedAvgReadDurationFile ecf mrfs
    let combinedAvgWriteDursFile = combinedAvgWriteDurationFile ecf mrfs
    let neededFiles =
            [ combinedResultsFile
            , combinedAvgDursFile
            , combinedAvgReadDursFile
            , combinedAvgWriteDursFile
            ]
    need neededFiles
    putLoud $ unwords ["Making my model from:", show neededFiles]
    setup <- readExperimentSetupForSummary $ head ers
    cres <- readCombinedClientsResults combinedResultsFile
    avgDurs <- readCombinedAvgDursFile combinedAvgDursFile
    avgReadDurs <- readCombinedAvgDursFile combinedAvgReadDursFile
    avgWriteDurs <- readCombinedAvgDursFile combinedAvgWriteDursFile
    (middleSetup, _) <-
        case backendSetup setup of
            Left _    -> fail "need middlesetup for my model."
            Right tup -> pure tup
    let unMiddleTime = (/ (10 ** 9))
    let parsingTime = avgAvgs (untilParsedTime avgDurs)
        enqueuingTime = avgAvgs (untilEnqueuedTime avgDurs)
    let nrReadThreads = fromIntegral $ mwNrThreads $ mMiddlewareFlags middleSetup
    -- Service time at acceptor = average parsing + hashing + enqueueing
    let accServiceTime = unMiddleTime $ parsingTime + enqueuingTime
    let readServiceTime =
            unMiddleTime $
            avgAvgs (untilAskedTime avgReadDurs) + avgAvgs (untilRepliedTime avgReadDurs) +
            avgAvgs (untilRespondedTime avgReadDurs)
    let writer1ServiceTime = unMiddleTime $ avgAvgs (untilAskedTime avgWriteDurs)
    let writer2ServiceTime = unMiddleTime $ avgAvgs (untilRepliedTime avgWriteDurs) + avgAvgs (untilRespondedTime avgWriteDurs)
    -- Arrival rate at acceptor = average throughput
    let overArr = avgAvgs $ avgBothResults $ avgTpsResults cres
    pure
        MyModel
        { overallArrivalRate = overArr
        , acceptorServiceTime = accServiceTime
        , getServiceTime = readServiceTime
        , getNrServers = nrReadThreads
        , setServiceTime = writer1ServiceTime
        , setIServiceTime = writer2ServiceTime
        }

evaluationRuleFor
    :: ExperimentConfig a
    => a -> String
evaluationRuleFor ecf = experimentTarget ecf ++ "-evaluate-my-model"

myModelEvaluationScript :: FilePath
myModelEvaluationScript = analysisDir </> "analyze_mymodel.r"

octaveScript :: FilePath
octaveScript = analysisDir </> "octave_in.oct"

octaveFunction :: FilePath
octaveFunction = analysisDir </> "analyze_mymodel.m"

mymodelSolutionFileFor :: ExperimentConfig a => a -> [FilePath] -> FilePath
mymodelSolutionFileFor ecf =
    changeFilename (const "my-model-solution") .
    (`replaceSndDir` experimentAnalysisTmpDir ecf) . head

evaluationRulesFor
    :: ExperimentConfig a
    => a -> Rules String
evaluationRulesFor ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    solutionFiles <- forM slocss $ \slocs -> do
        let solutionFile = mymodelSolutionFileFor ecf  slocs
        solutionFile %> \_ -> do
            let modelFile = myModelEstimateFileFor ecf slocs
            need [modelFile, rBin, commonRLib, myModelEvaluationScript, octaveScript, octaveFunction]
            needRLibs ["R.matlab", "RJSONIO"]
            putLoud $ unwords ["Solving my model in", modelFile, "and dumping the result in", solutionFile]
            solution <- solveMyModel ecf slocs
            writeJSON solutionFile solution
        pure solutionFile

    let rule = evaluationRuleFor ecf
    rule ~> need solutionFiles
    pure rule

solveMyModel :: ExperimentConfig a => a -> [FilePath] -> Action MyModelSolution
solveMyModel ecf slocs = do
    ers <- readResultsSummary $ head slocs
    setup <- readExperimentSetupForSummary ers
    (middleSetup, serverSetups) <-
        case backendSetup setup of
            Left _    -> fail "need middlesetup for my model."
            Right tup -> pure tup
    let modelFile = myModelEstimateFileFor ecf slocs
    MyModel {..} <- readMyModelFile modelFile
    let tmpResultDir = "/tmp/octave_out"
        outResultDir = "/tmp/model_out"
    unit $ cmd "mkdir" "--parents" tmpResultDir
    unit $ cmd "mkdir" "--parents" outResultDir
    let tmpResultPath = tmpResultDir </> show (hash slocs)
        outResultPath = outResultDir </> show (hash slocs)
    let nrSers = length serverSetups
    let nrThds = mwNrThreads $ mMiddlewareFlags middleSetup
    let writeProp = setProportion $ msConfig $ cMemaslapSettings $ head $ clientSetups setup
    unless (getNrServers == fromIntegral nrThds) $ fail "wut."
    unit $
        rScript
            myModelEvaluationScript
            commonRLib
            tmpResultPath
            outResultPath
            (show nrSers)
            (show nrThds)
            (show writeProp)
            (show overallArrivalRate)
            (show acceptorServiceTime)
            (show getServiceTime)
            (show setServiceTime)
            (show setIServiceTime)
    resultByOctave <- readJSON outResultPath :: Action ByOctaveMyModelSolution
    pure $ unOctave resultByOctave

myModelTexFilePrefix
    :: ExperimentConfig a
    => a -> FilePath
myModelTexFilePrefix ecf = reportsTmpDir </> experimentTarget ecf ++ "-mymodel" <.> texExt

myModelModelTexFileWithPostfix
    :: ExperimentConfig a
    => a -> String -> FilePath
myModelModelTexFileWithPostfix ecf postfix =
    changeFilename (++ "-" ++ postfix) $ myModelTexFilePrefix ecf

myModelTexFiles
    :: ExperimentConfig a
    => a -> [FilePath]
myModelTexFiles ecf = [myModelModelTexFileWithPostfix ecf "model"]

myModelFileForReport :: FilePath -> Int -> FilePath
myModelFileForReport file i = file `replaceDirectory` modelDirForReport i

useMyModelInReport
    :: ExperimentConfig a
    => a -> Int -> Rules ()
useMyModelInReport ecf i =
    forM_ (myModelTexFiles ecf) $ \eff -> myModelFileForReport eff i `byCopying` eff

dependOnMyModelForReport
    :: ExperimentConfig a
    => a -> Int -> Action ()
dependOnMyModelForReport ecf i = need $ map (`myModelFileForReport` i) $ myModelTexFiles ecf

texfileRuleFor
    :: ExperimentConfig a
    => a -> String
texfileRuleFor ecf = experimentTarget ecf ++ "-mymodel-texfiles"

texfileRulesFor
    :: ExperimentConfig a
    => a -> Rules String
texfileRulesFor ecf = do
    let texfiles = myModelTexFiles ecf
    texfiles &%> \_ -> genTexfilesFor ecf
    let rule = texfileRuleFor ecf
    rule ~> need texfiles
    pure rule

genTexfilesFor
    :: ExperimentConfig a
    => a -> Action ()
genTexfilesFor ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    slocs <-
        case slocss of
            [x] -> pure x
            _   -> fail "Need exactly one model for Mymodel texfiles."
    let myModelFile = myModelEstimateFileFor ecf slocs
    need [myModelFile]
    MyModel {..} <- readMyModelFile myModelFile
    let unmodeltime = (* (1000 * 1000))
    let tab =
            tabularWithHeader
                ["Measure", "Value", "Unit"]
                [ ["Arrival rate", printf "%.f" overallArrivalRate, "transactions / second"]
                , [ "Acceptor service time"
                  , printf "%.f" $ unmodeltime acceptorServiceTime
                  , "$\\mu s$"
                  ]
                , [ "Read worker service time"
                  , printf "%.f" $ unmodeltime getServiceTime
                  , "$\\mu s$"
                  ]
                , ["Nr of reader servers", printf "%.f" getNrServers, "Servers"]
                , [ "Write sender service time"
                  , printf "%.f" $ unmodeltime setServiceTime
                  , "$\\mu s$"
                  ]
                , [ "Write receiver service time"
                  , printf "%.f" $ unmodeltime setIServiceTime
                  , "$\\mu s$"
                  ]
                ]
    writeFile' (myModelModelTexFileWithPostfix ecf "model") tab
