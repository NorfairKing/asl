module AslBuild.LocalMiddlewareThoroughTest where

import           Data.List                    (intercalate)

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.LocalMiddlewareTest
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Middleware
import           AslBuild.Types

localMiddlewareThoroughTestRule :: String
localMiddlewareThoroughTestRule = "local-middleware-thorough-test"

setups :: [LocalMiddlewareTestSetup]
setups = do
    let rtime = 1

    nrServers <- [2, 8]
    let serverFlags = do
            port <- take nrServers [11211 ..]
            return MemcachedFlags
                { memcachedPort = port
                , memcachedAsDaemon = False
                }

    nrThreads <- [1, 2]
    replicationFactor <- takeWhile (<= nrServers) $ iterate (*2) 1

    let mwFlags = MiddlewareFlags
            { mwIp = localhostIp
            , mwPort = 11210
            , mwNrThreads = nrThreads
            , mwReplicationFactor = replicationFactor
            , mwServers = map (RemoteServerUrl localhostIp . memcachedPort) serverFlags
            , mwVerbosity = LogFine
            , mwTraceFile = tmpDir </> localMiddlewareThoroughTestRule ++ "-trace" <.> csvExt
            }

    nrClients <- [2, 8]
    keySize <- [16, 16]
    valueSize <- [128, 1024]
    threads <- [1, 2]
    -- Concurrency must be a multiple of thread count.
    concurrency <- (* threads) <$> [16, 64]

    setProp <- [0.1]
    nrRequests <- [256, 512]

    let signature = intercalate "-"
            [ show nrClients
            , show nrServers
            , show nrThreads
            , show replicationFactor
            , show keySize
            , show valueSize
            , show threads
            , show concurrency
            , show setProp
            , show nrRequests
            ]

    let mconfig = MemaslapConfig
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
            , msWorkload = NrRequests nrRequests
            , msStatFreq = Nothing
            , msConfigFile = tmpDir
                </> "local-middleware-thorough-test"
                </> "local-middleware-thorough-test-memaslap-cfg-" ++ signature
            }

    let msSets = MemaslapSettings
            { msConfig = mconfig
            , msFlags = flags
            }

    return LocalMiddlewareTestSetup
        { runtime = rtime
        , clientSetups = replicate nrClients msSets
        , middlewareSetup = mwFlags
        , serverSetups = serverFlags
        }

localMiddlewareThoroughTestRules :: Rules ()
localMiddlewareThoroughTestRules =
    localMiddlewareThoroughTestRule ~> runLocalMiddlewareTests setups
