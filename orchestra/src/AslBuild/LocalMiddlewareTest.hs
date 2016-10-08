{-# LANGUAGE RecordWildCards #-}
module AslBuild.LocalMiddlewareTest where

import           Control.Concurrent
import           Control.Monad
import           Data.List                  (intercalate)

import           System.Exit
import           System.Process

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.Memaslap
import           AslBuild.Memcached
import           AslBuild.Middleware
import           AslBuild.Types

localMiddlewareTestRule :: String
localMiddlewareTestRule = "local-middleware-test"


data LocalMiddlewareTestSetup
    = LocalMiddlewareTestSetup
    { runtime          :: Int
    , memaslapSettings :: MemaslapSettings
    , middlewareFlags  :: MiddlewareFlags
    , memcachedFlags   :: MemcachedFlags
    } deriving (Show, Eq)

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

    keySize <- [16, 32, 128]
    valueSize <- [16, 256, 4096]
    threads <- [1, 2]
    -- Concurrency must be a multiple of thread count.
    concurrency <- (* threads) <$> [1, 2]

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
        , memaslapSettings = msSets
        , middlewareFlags = mwFlags
        , memcachedFlags = mcFlags
        }

localMiddlewareTestRules :: Rules ()
localMiddlewareTestRules =
    localMiddlewareTestRule ~> do
        need [memcachedBin, memaslapBin, outputJarFile]
        forM_ setups $ \LocalMiddlewareTestSetup{..} -> do
            writeMemaslapConfig (msConfigFile $ msFlags memaslapSettings) $ msConfig memaslapSettings

            serverPH <- cmd memcachedBin
                (memcachedArgs memcachedFlags)

            middlePH <- cmd javaCmd
                "-jar" outputJarFile
                (middlewareArgs middlewareFlags)

            waitMs 250

            clientPH <- cmd memaslapBin
                (memaslapArgs $ msFlags memaslapSettings)

            let terminateAll = do
                    terminateProcess clientPH
                    terminateProcess middlePH
                    terminateProcess serverPH

            let goOn = do
                    wait runtime
                    putLoud "Done waiting, killing processes!"

                    clc <- liftIO $ getProcessExitCode clientPH
                    case clc of
                        Just (ExitFailure ec) ->
                            fail $ "client failed with exitcode: " ++ show ec
                        _ -> return ()

                    mec <- liftIO $ getProcessExitCode middlePH
                    case mec of
                        Just (ExitFailure ec) ->
                            fail $ "Middleware failed with exitcode: " ++ show ec
                        _ -> return ()

            actionFinally goOn $ do
                terminateAll
                threadDelay $ 1 * 1000 * 1000 -- Wait for everything to grind to a halt.

