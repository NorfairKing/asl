{-# LANGUAGE RecordWildCards #-}
module AslBuild.LocalMiddlewareTest where

import           Control.Monad
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
    let time = 5

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

    let msSets = MemaslapSettings
            { msConfig = MemaslapConfig
                { keysizeDistributions = [Distribution 128 128 1]
                , valueDistributions = [Distribution 2048 2048 1]
                , setProportion = 0.1
                , getProportion = 0.9
                }
            , msFlags = MemaslapFlags
                { msServers = [RemoteServerUrl (mwIp mwFlags) (mwPort mwFlags)]
                , msThreads = 1
                , msConcurrency = 1
                , msOverwrite = 1
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
localMiddlewareTestRules = do
    localMiddlewareTestRule ~> do
        need [memcachedBin, memaslapBin, outputJarFile]
        forM_ setups $ \LocalMiddlewareTestSetup{..} -> do
            writeMemaslapConfig (msConfigFile $ msFlags memaslapSettings) $ msConfig memaslapSettings

            serverPH <- cmd memcachedBin
                (memcachedArgs memcachedFlags)

            wait 1

            middlePH <- cmd javaCmd
                "-jar" outputJarFile
                (middlewareArgs middlewareFlags)

            wait 1

            clientPH <- cmd memaslapBin
                (memaslapArgs $ msFlags memaslapSettings)

            wait runtime

            liftIO $ do
                terminateProcess clientPH
                terminateProcess middlePH
                terminateProcess serverPH


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
