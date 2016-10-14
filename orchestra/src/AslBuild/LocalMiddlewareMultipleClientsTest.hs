module AslBuild.LocalMiddlewareMultipleClientsTest where

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
    let time = 1

    let serverFlags = MemcachedFlags
            { memcachedPort = defaultMemcachedPort
            , memcachedAsDaemon = False
            }

    let mwFlags = MiddlewareFlags
            { mwIp = localhostIp
            , mwPort = 11210
            , mwNrThreads = 1
            , mwReplicationFactor = 1
            , mwServers = [RemoteServerUrl localhostIp $ memcachedPort serverFlags]
            , mwVerbosity = LogFine
            , mwTraceFile = tmpDir </> localMiddlewareMultipleClientsTestRule ++ "-trace" <.> csvExt
            }

    nrClients <- [2, 3, 5, 10]
    keySize <- [128]
    valueSize <- [1024]
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

    let config = MemaslapConfig
            { keysizeDistributions = [Distribution keySize keySize 1]
            , valueDistributions = [Distribution valueSize valueSize 1]
            , setProportion = setProp
            , getProportion = 1 - setProp
            }

    let flags = MemaslapFlags
            { msServers = [RemoteServerUrl (mwIp mwFlags) (mwPort mwFlags)]
            , msThreads = threads
            , msConcurrency = concurrency
            , msOverwrite = 0.5
            , msWorkload = NrRequests 256
            , msStatFreq = Nothing
            , msConfigFile = tmpDir
                </> "local-middleware-multiple-clients-test"
                </> "local-middleware-multiple-clients-test-memaslap-cfg-" ++ signature
            }

    let msSets = MemaslapSettings
            { msConfig = config
            , msFlags = flags
            }

    return LocalMiddlewareTestSetup
        { runtime = time
        , clientSetups = replicate nrClients msSets
        , middlewareSetup = mwFlags
        , serverSetups = [serverFlags]
        }

localMiddlewareMultipleClientsTestRules :: Rules ()
localMiddlewareMultipleClientsTestRules =
    localMiddlewareMultipleClientsTestRule ~> runLocalMiddlewareTests setups
