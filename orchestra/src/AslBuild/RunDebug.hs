module AslBuild.RunDebug where

import Control.Monad

import System.Process

import Development.Shake
import Development.Shake.FilePath

import AslBuild.CommonActions
import AslBuild.Constants
import AslBuild.Memaslap
import AslBuild.Memcached
import AslBuild.Middleware
import AslBuild.Types

runDebugRule :: String
runDebugRule = "run-debug"

runDebugRules :: Rules ()
runDebugRules =
    runDebugRule ~> do
        let mcfs = MemcachedFlags {memcachedPort = 11211, memcachedAsDaemon = False}
        let mwfs =
                MiddlewareFlags
                { mwIp = localhostIp
                , mwPort = 11212
                , mwNrThreads = 1
                , mwReplicationFactor = 1
                , mwServers = [RemoteServerUrl localhostIp $ memcachedPort mcfs]
                , mwVerbosity = LogFine
                , mwTraceFile = tmpDir </> "debug-trace" <.> csvExt
                , mwReadSampleRate = Nothing
                , mwWriteSampleRate = Nothing
                }
        let cfs =
                MemaslapSettings
                { msConfig =
                      MemaslapConfig
                      { keysizeDistributions = [Distribution 16 16 1]
                      , valueDistributions = [Distribution 128 128 1]
                      , setProportion = 0.01
                      }
                , msFlags =
                      MemaslapFlags
                      { msServers = [RemoteServerUrl localhostIp $ mwPort mwfs]
                      , msThreads = 1
                      , msConcurrency = 1
                      , msOverwrite = 0.9
                      , msStatFreq = Just $ Seconds 1
                      , msWorkload = WorkFor $ Hours 1
                      , msWindowSize = Kilo 1
                      , msConfigFile = tmpDir </> "debug-memaslap-config" <.> txtExt
                      }
                }
        writeMemaslapConfig (msConfigFile $ msFlags cfs) (msConfig cfs)
        serverPh <- runMemcachedLocally mcfs
        waitMs 100
        middlePh <- runMiddlewareLocally mwfs
        waitMs 100
        clientPh <- runMemaslapLocally $ msFlags cfs
        actionFinally (forever $ wait 1) $ do
            terminateProcess clientPh
            void $ waitForProcess clientPh
            terminateProcess middlePh
            void $ waitForProcess middlePh
            terminateProcess serverPh
            void $ waitForProcess serverPh
