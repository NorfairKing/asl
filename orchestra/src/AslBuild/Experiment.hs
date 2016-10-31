{-# LANGUAGE RecordWildCards #-}
module AslBuild.Experiment
    ( module AslBuild.Experiment
    , module AslBuild.Experiment.Types
    ) where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           System.Process

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.BuildMemcached
import           AslBuild.Client
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiment.Types
import           AslBuild.Jar
import           AslBuild.Memaslap
import           AslBuild.Middle
import           AslBuild.Provision
import           AslBuild.Server
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm

generateTargetFor
    :: ExperimentConfig a
    => a
    -> Rules ()
generateTargetFor ecf = do
    let rFile = resultSummariesLocationFile ecf

    experimentTarget ecf ~> need [rFile]

    rFile %> \_ -> do
        need [memcachedBin, memaslapBin, outputJarFile]
        need [provisionLocalhostRule]

        (eSetups, vmsNeeded) <- genExperimentSetups ecf

        provisionVmsFromData vmsNeeded

        forM_ eSetups $ \ExperimentSetup{..} -> do
            -- Get the clients configs set up
            setupClientConfigs clientSetups

            -- Start the servers
            startServersOn serverSetups

            -- Wait for the servers to get started
            wait 1

            -- Start the middleware
            middlePh <- startMiddleOn middleSetup

            -- Wait for the middleware to get started
            wait 1

            -- Start the clients
            startClientsOn clientSetups

            -- Wait for the experiment to finish
            actionFinally (waitNicely $ toSeconds esRuntime) (return ())

            -- Shut down the middleware
            shutdownMiddle middleSetup
            void $ liftIO $ waitForProcess middlePh

            -- Shut down the servers
            shutdownServers serverSetups

            -- Copy the middleware logs back
            copyMiddleTraceBack middleSetup

            -- Wait for memaslap to finish writing logs.
            wait 1

            -- Shut down the memaslap instances
            shutdownClients clientSetups

            -- Copy the client logs back
            copyClientLogsBack clientSetups

            -- Prepare analysis files for the client logs.
            makeClientResultFiles clientSetups

            -- Make the result record
            let results = ExperimentResultSummary
                    { erClientResults = map cResultsFile clientSetups
                    , erMiddleResults = mLocalTrace middleSetup
                    }

            -- Write the result record to file
            writeJSON esResultsSummaryFile results

        writeJSON rFile $ map esResultsSummaryFile eSetups

resultSummariesLocationFile :: ExperimentConfig a => a -> FilePath
resultSummariesLocationFile cfg
    = resultsDir </> experimentTarget cfg ++ "-summary-locations" <.> jsonExt

makeClientResultFiles :: [ClientSetup] -> Action ()
makeClientResultFiles = (`forP_` makeClientResultFile)

makeClientResultFile :: ClientSetup -> Action ()
makeClientResultFile cSetup@ClientSetup{..} = do
    mel <- parseLog cLocalLog
    case mel of
        Nothing -> fail $ "could not parse logfile: " ++ cLocalLog
        Just parsedLog -> do
            let results = ClientResults
                    { crSetup = cSetup
                    , crLog = parsedLog
                    }
            writeJSON cResultsFile results

waitNicely :: Int -> Action ()
waitNicely is = do
    putLoud $ "Waiting for " ++ toClockString is
    go is
  where
    period = 10
    go s = do
        putLoud $ toClockString s ++ " remaining."
        if s < period
        then liftIO $ threadDelay $ s * 1000 * 1000
        else do
            liftIO $ threadDelay $ period * 1000 * 1000
            go $ s - period

getVmsForExperiments
    :: ExperimentConfig a
    => a
    -> Action ([(RemoteLogin, String)], [(RemoteLogin, String)], [(RemoteLogin, String)], [VmData])
getVmsForExperiments ecf = do
    let HighLevelConfig{..} = highLevelConfig ecf
    case location of
        Local -> do
            let localLogin = RemoteLogin Nothing localhostIp
            let localPrivate = localhostIp
            let localTup = (localLogin, localPrivate)
            let locals = repeat localTup
            return (take nrClients locals, take 1 locals, take nrServers locals, [])
        Remote -> do
            (cs, ms, ss) <- getVms nrClients 1 nrServers
            let login VmData{..} = RemoteLogin (Just vmAdmin) vmFullUrl
            let private VmData{..} = vmPrivateIp
            let tups = map (login &&& private)
            return (tups cs, tups ms, tups ss, cs ++ ms ++ ss)
