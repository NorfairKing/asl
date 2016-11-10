module AslBuild.LocalMiddlewareTest.MultipleClientsTest where

import           Data.List                    (intercalate)

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.LocalMiddlewareTest
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Middleware
import           AslBuild.Types

localMiddlewareMultipleClientsTestRule :: String
localMiddlewareMultipleClientsTestRule = "local-middleware-multiple-clients-test"

setups :: [LocalMiddlewareTestSetup]
setups = do
    let rtime = 1

    let serverFlags = MemcachedFlags
            { memcachedPort = 11248
            , memcachedAsDaemon = False
            }

    nrClients <- [2, 6 .. 20]
    keySize <- [16]
    valueSize <- [128]
    threads <- [2]
    -- Concurrency must be a multiple of thread count.
    concurrency <- (* threads) <$> [2]

    setProp <- [0.1]

    let signature = intercalate "-"
            [ show nrClients
            , show keySize
            , show valueSize
            , show threads
            , show concurrency
            , show setProp
            ]

    let mwFlags = MiddlewareFlags
            { mwIp = localhostIp
            , mwPort = 11249
            , mwNrThreads = 1
            , mwReplicationFactor = 1
            , mwServers = [RemoteServerUrl localhostIp $ memcachedPort serverFlags]
            , mwVerbosity = LogFine
            , mwTraceFile = tmpDir
                </> localMiddlewareMultipleClientsTestRule
                </> localMiddlewareMultipleClientsTestRule ++ "-trace-" ++ signature <.> csvExt
            , mwReadSampleRate = Nothing
            , mwWriteSampleRate = Nothing
            }

    let mconfig = MemaslapConfig
            { keysizeDistributions = [Distribution keySize keySize 1]
            , valueDistributions = [Distribution valueSize valueSize 1]
            , setProportion = setProp
            }

    let flags = MemaslapFlags
            { msServers = [RemoteServerUrl (mwIp mwFlags) (mwPort mwFlags)]
            , msThreads = threads
            , msConcurrency = concurrency
            , msOverwrite = 0.5
            , msWorkload = NrRequests 256
            , msStatFreq = Nothing
            , msWindowSize = Kilo 1
            , msConfigFile = tmpDir
                </> "local-middleware-multiple-clients-test"
                </> "local-middleware-multiple-clients-test-memaslap-cfg-" ++ signature
            }

    let msSets = MemaslapSettings
            { msConfig = mconfig
            , msFlags = flags
            }

    return LocalMiddlewareTestSetup
        { runtime = rtime
        , clientSetups = replicate nrClients msSets
        , middlewareSetup = mwFlags
        , serverSetups = [serverFlags]
        }

localMiddlewareMultipleClientsTestRules :: Rules ()
localMiddlewareMultipleClientsTestRules =
    localMiddlewareMultipleClientsTestRule ~> runLocalMiddlewareTests setups
