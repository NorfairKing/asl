{-# LANGUAGE RecordWildCards #-}
module AslBuild.LocalMiddlewareTest where

import           Control.Monad
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
import           AslBuild.Types

localMiddlewareTestRule :: String
localMiddlewareTestRule = "local-test"


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
            }

    keySize <- [16, 32, 128]
    valueSize <- [16, 32, 128]
    threads <- [1, 2]
    -- Concurrency must be a multiple of thread count.
    concurrency <- (* threads) <$> [1, 2]

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
                , msConfigFile = tmpDir </> "local-middleware-test-memaslap-cfg.txt"
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

            let tmpStdErr = "/tmp/middle_std_err"
            let tmpStdOut = "/tmp/middle_std_out"
            middlePH <- cmd javaCmd
                (FileStderr tmpStdErr)
                (FileStdout tmpStdOut)
                "-jar" outputJarFile
                (middlewareArgs middlewareFlags)

            clientPH <- cmd memaslapBin
                (memaslapArgs $ msFlags memaslapSettings)

            wait runtime
            putLoud "Done waiting, killing processes!"

            mec <- liftIO $ getProcessExitCode middlePH
            let terminateAll = liftIO $ do
                    terminateProcess clientPH
                    terminateProcess middlePH
                    terminateProcess serverPH
            case mec of
                Just (ExitFailure ec) -> do
                    liftIO $ do
                        sout <- readFile tmpStdOut
                        putStrLn sout
                        serr <- readFile tmpStdErr
                        putStrLn serr
                    terminateAll
                    fail $ "Middleware failed with exitcode: " ++ show ec
                _ -> return ()

            terminateAll


data MiddlewareFlags
    = MiddlewareFlags
    { mwIp                :: String
    , mwPort              :: Int
    , mwNrThreads         :: Int
    , mwReplicationFactor :: Int
    , mwServers           :: [RemoteServerUrl]
    } deriving (Show, Eq)

middlewareArgs :: MiddlewareFlags -> [String]
middlewareArgs MiddlewareFlags{..} =
    [ "-l", mwIp
    , "-p", show mwPort
    , "-t", show mwNrThreads
    , "-r", show mwReplicationFactor
    , "-m", unwords $ map remoteServerUrl mwServers
    ]
