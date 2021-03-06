module AslBuild.LocalMiddlewareTest.MultipleServersTest where

import Data.List (intercalate)

import Development.Shake
import Development.Shake.FilePath

import AslBuild.Constants
import AslBuild.LocalMiddlewareTest
import AslBuild.Memaslap
import AslBuild.Memcached
import AslBuild.Middleware
import AslBuild.Types

localMiddlewareMultipleServersTestRule :: String
localMiddlewareMultipleServersTestRule = "local-middleware-multiple-servers-test"

setups :: [LocalMiddlewareTestSetup]
setups = do
    let rtime = 1
    nrServers <- [2, 3, 5, 10]
    let serverFlags = do
            port <- take nrServers [11250 ..]
            return MemcachedFlags {memcachedPort = port, memcachedAsDaemon = False}
    let mwFlags =
            MiddlewareFlags
            { mwIp = localhostIp
            , mwPort = 11261
            , mwNrThreads = 1
            , mwReplicationFactor = 1
            , mwServers = map (RemoteServerUrl localhostIp . memcachedPort) serverFlags
            , mwVerbosity = LogFine
            , mwTraceFile = tmpDir </> localMiddlewareMultipleServersTestRule ++ "-trace" <.> csvExt
            , mwReadSampleRate = Nothing
            , mwWriteSampleRate = Nothing
            }
    keySize <- [128]
    valueSize <- [1024]
    threads <- [2]
    -- Concurrency must be a multiple of thread count.
    concurrency <- (* threads) <$> [2]
    setProp <- [0.1]
    let signature =
            intercalate
                "-"
                [ show nrServers
                , show keySize
                , show valueSize
                , show threads
                , show concurrency
                , show setProp
                ]
    let msSets =
            MemaslapSettings
            { msConfig =
                  MemaslapConfig
                  { keysizeDistributions = [Distribution keySize keySize 1]
                  , valueDistributions = [Distribution valueSize valueSize 1]
                  , setProportion = setProp
                  }
            , msFlags =
                  MemaslapFlags
                  { msServers = [RemoteServerUrl (mwIp mwFlags) (mwPort mwFlags)]
                  , msThreads = threads
                  , msConcurrency = concurrency
                  , msOverwrite = 0.9
                  , msWorkload = NrRequests 256
                  , msStatFreq = Nothing
                  , msWindowSize = Kilo 1
                  , msConfigFile =
                        tmpDir </> "local-middleware-multiple-servers-test" </>
                        "local-middleware-multiple-servers-test-memaslap-cfg-" ++
                        signature
                  }
            }
    return
        LocalMiddlewareTestSetup
        { runtime = rtime
        , clientSetups = [msSets]
        , middlewareSetup = mwFlags
        , serverSetups = serverFlags
        }

localMiddlewareMultipleServersTestRules :: Rules ()
localMiddlewareMultipleServersTestRules =
    localMiddlewareMultipleServersTestRule ~> runLocalMiddlewareTests setups
