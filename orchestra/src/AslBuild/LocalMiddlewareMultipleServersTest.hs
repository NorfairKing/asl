{-# LANGUAGE RecordWildCards #-}
module AslBuild.LocalMiddlewareMultipleServersTest where

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

localMiddlewareMultipleServersTestRule :: String
localMiddlewareMultipleServersTestRule = "local-middleware-multiple-servers-test"

data LocalMiddlewareMultipleServersTestSetup
    = LocalMiddlewareMultipleServersTestSetup
    { runtime          :: Int
    , memaslapSettings :: MemaslapSettings
    , middlewareFlags  :: MiddlewareFlags
    , memcachedFlagss  :: [MemcachedFlags]
    } deriving (Show, Eq)

setups :: [LocalMiddlewareMultipleServersTestSetup]
setups = do
    let time = 1

    nrServers <- [2, 3, 5, 10]
    let serverFlags = do
            port <- take nrServers [11211 ..]
            return MemcachedFlags
                { memcachedPort = port
                , memcachedAsDaemon = False
                }

    let mwFlags = MiddlewareFlags
            { mwIp = "localhost"
            , mwPort = 11210
            , mwNrThreads = 1
            , mwReplicationFactor = 1
            , mwServers = map (RemoteServerUrl "localhost" . memcachedPort) serverFlags
            , mwVerbosity = LogFine
            }

    keySize <- [16, 128]
    valueSize <- [16, 4096]
    threads <- [1, 2]
    -- Concurrency must be a multiple of thread count.
    concurrency <- (* threads) <$> [1, 2]

    setProp <- [0.1]

    let signature = intercalate "-"
            [ show nrServers
            , show keySize
            , show valueSize
            , show threads
            , show concurrency
            , show setProp
            ]

    let msSets = MemaslapSettings
            { msConfig = MemaslapConfig
                { keysizeDistributions = [Distribution keySize keySize 1]
                , valueDistributions = [Distribution valueSize valueSize 1]
                , setProportion = setProp
                , getProportion = 1 - setProp
                }
            , msFlags = MemaslapFlags
                { msServers = [RemoteServerUrl (mwIp mwFlags) (mwPort mwFlags)]
                , msThreads = threads
                , msConcurrency = concurrency
                , msOverwrite = 0.5
                , msStatFreq = Seconds $ time + 2
                , msTime = Seconds $ time + 2
                , msConfigFile = tmpDir
                    </> "local-middleware-multiple-servers-test"
                    </> "local-middleware-multiple-servers-test-memaslap-cfg-" ++ signature
                }
            }

    return LocalMiddlewareMultipleServersTestSetup
        { runtime = time
        , memaslapSettings = msSets
        , middlewareFlags = mwFlags
        , memcachedFlagss = serverFlags
        }

localMiddlewareMultipleServersTestRules :: Rules ()
localMiddlewareMultipleServersTestRules =
    localMiddlewareMultipleServersTestRule ~> do
        need [memcachedBin, memaslapBin, outputJarFile]
        forM_ setups $ \LocalMiddlewareMultipleServersTestSetup{..} -> do
            writeMemaslapConfig (msConfigFile $ msFlags memaslapSettings) $ msConfig memaslapSettings

            serverPHs <- forM memcachedFlagss $ \memcachedFlags ->
                cmd memcachedBin (memcachedArgs memcachedFlags)

            middlePH <- cmd javaCmd
                "-jar" outputJarFile
                (middlewareArgs middlewareFlags)

            waitMs 250

            clientPH <- cmd memaslapBin
                (memaslapArgs $ msFlags memaslapSettings)

            let terminateAll = do
                    terminateProcess clientPH
                    terminateProcess middlePH
                    mapM_ terminateProcess serverPHs

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

