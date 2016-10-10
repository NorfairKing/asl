module AslBuild.LocalMiddlewareSimpleTest where

import           Data.List                    (intercalate)

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.LocalMiddlewareTest
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Middleware
import           AslBuild.Types

localMiddlewareSimpleTestRule :: String
localMiddlewareSimpleTestRule = "local-middleware-simple-test"

setups :: [LocalMiddlewareTestSetup]
setups = do
    let time = 1

    let mcFlags = MemcachedFlags
            { memcachedPort = defaultMemcachedPort
            , memcachedAsDaemon = False
            }
    let mwFlags = MiddlewareFlags
            { mwIp = "localhost"
            , mwPort = 11234
            , mwNrThreads = 1
            , mwReplicationFactor = 1
            , mwServers = [RemoteServerUrl "localhost" 11211]
            , mwVerbosity = LogFine
            }

    keySize <- [16, 128]
    valueSize <- [16, 4096]
    threads <- [2]
    -- Concurrency must be a multiple of thread count.
    concurrency <- (* threads) <$> [2]

    let signature = intercalate "-"
            [ show keySize
            , show valueSize
            , show threads
            , show concurrency
            ]

    let msSets = MemaslapSettings
            { msConfig = MemaslapConfig
                { keysizeDistributions = [Distribution keySize keySize 1]
                , valueDistributions = [Distribution valueSize valueSize 1]
                , setProportion = 0.1
                , getProportion = 0.9
                }
            , msFlags = MemaslapFlags
                { msServers = [RemoteServerUrl (mwIp mwFlags) (mwPort mwFlags)]
                , msThreads = threads
                , msConcurrency = concurrency
                , msOverwrite = 0.5
                , msStatFreq = Seconds $ time + 2
                , msTime = Seconds $ time + 2
                , msConfigFile = tmpDir
                    </> "local-middleware-test"
                    </> "local-middleware-test-memaslap-cfg-" ++ signature
                }
            }

    return LocalMiddlewareTestSetup
        { runtime = time
        , clientSetups = [msSets]
        , middlewareSetup = mwFlags
        , serverSetups = [mcFlags]
        }

localMiddlewareSimpleTestRules :: Rules ()
localMiddlewareSimpleTestRules = localMiddlewareSimpleTestRule ~> runLocalMiddlewareTests setups
