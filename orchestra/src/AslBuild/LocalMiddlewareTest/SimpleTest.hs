module AslBuild.LocalMiddlewareTest.SimpleTest where

import Data.List (intercalate)

import Development.Shake
import Development.Shake.FilePath

import AslBuild.Constants
import AslBuild.LocalMiddlewareTest
import AslBuild.Memaslap
import AslBuild.Memcached
import AslBuild.Middleware
import AslBuild.Types

localMiddlewareSimpleTestRule :: String
localMiddlewareSimpleTestRule = "local-middleware-simple-test"

setups :: [LocalMiddlewareTestSetup]
setups = do
    let rtime = 1
    let sPort = defaultMemcachedPort
    let mcFlags = MemcachedFlags {memcachedPort = sPort, memcachedAsDaemon = False}
    let mwFlags =
            MiddlewareFlags
            { mwIp = localhostIp
            , mwPort = defaultMemcachedPort + 1
            , mwNrThreads = 1
            , mwReplicationFactor = 1
            , mwServers = [RemoteServerUrl localhostIp sPort]
            , mwVerbosity = LogFine
            , mwTraceFile = tmpDir </> localMiddlewareSimpleTestRule ++ "-trace" <.> csvExt
            , mwReadSampleRate = Nothing
            , mwWriteSampleRate = Nothing
            }
    keySize <- [16, 32]
    valueSize <- [16, 128]
    threads <- [2]
    -- Concurrency must be a multiple of thread count.
    concurrency <- (* threads) <$> [2]
    let signature = intercalate "-" [show keySize, show valueSize, show threads, show concurrency]
    let msSets =
            MemaslapSettings
            { msConfig =
                  MemaslapConfig
                  { keysizeDistributions = singleDist keySize
                  , valueDistributions = singleDist valueSize
                  , setProportion = 0.1
                  }
            , msFlags =
                  MemaslapFlags
                  { msServers = [RemoteServerUrl (mwIp mwFlags) (mwPort mwFlags)]
                  , msThreads = threads
                  , msConcurrency = concurrency
                  , msOverwrite = 0.5
                  , msWorkload = NrRequests 256
                  , msStatFreq = Nothing
                  , msWindowSize = Kilo 1
                  , msConfigFile =
                        tmpDir </> "local-middleware-simple-test" </>
                        "local-middleware-simple-test-memaslap-cfg-" ++
                        signature
                  }
            }
    return
        LocalMiddlewareTestSetup
        { runtime = rtime
        , clientSetups = [msSets]
        , middlewareSetup = mwFlags
        , serverSetups = [mcFlags]
        }

localMiddlewareSimpleTestRules :: Rules ()
localMiddlewareSimpleTestRules = localMiddlewareSimpleTestRule ~> runLocalMiddlewareTests setups
