{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.LocalMiddlewareTest.ReplicationFailureTest where

import           Control.Monad
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as SB
import qualified Data.ByteString.Char8      as SB8
import           Data.Monoid
import           Network.Socket             hiding (recv, send)
import           Network.Socket.ByteString
import           System.Process
import           System.Timeout

import           Control.Concurrent.Thread

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.Middleware
import           AslBuild.Types

data ReplicationFailureTestSetup
    = ReplicationFailureTestSetup
    { cPort     :: Int
    , mPort     :: Int
    , sPortBase :: Int
    , nrServers :: Int
    , err       :: ServerError
    } deriving (Show, Eq)

data ServerError = NOT_STORED | SERVER_ERROR
    deriving (Show, Eq)

setups :: [ReplicationFailureTestSetup]
setups = do
    nSers <- takeWhile (<= 8) $ iterate (*2) 2
    e <- [NOT_STORED, SERVER_ERROR]
    return ReplicationFailureTestSetup
        { cPort = 11272
        , mPort = 11273
        , sPortBase = 11274
        , nrServers = nSers
        , err = e
        }

localMiddlewareReplicationFailureTestRule :: String
localMiddlewareReplicationFailureTestRule = "local-middleware-replication-failure-test"

localMiddlewareReplicationFailureTestRules :: Rules ()
localMiddlewareReplicationFailureTestRules =
    localMiddlewareReplicationFailureTestRule ~> do
        need [outputJarFile]

        forM_ setups $ \ReplicationFailureTestSetup{..} -> do

            let sixs = [0 .. (nrServers - 1)]
            let mwFlags = MiddlewareFlags
                    { mwIp = localhostIp
                    , mwPort = mPort
                    , mwNrThreads = 1
                    , mwReplicationFactor = nrServers
                    , mwServers = map (RemoteServerUrl localhostIp . (+ sPortBase)) sixs
                    , mwVerbosity = LogAll
                    , mwTraceFile = tmpDir </> localMiddlewareReplicationFailureTestRule ++ "-trace" <.> csvExt
                    , mwReadSampleRate = Nothing
                    , mwWriteSampleRate = Nothing
                    }

            ssockss <- liftIO $ forM sixs $ \ix -> do
                -- Create server socket
                sock <- socket AF_INET Stream defaultProtocol
                -- Make it immediately available
                setSocketOption sock ReuseAddr 1
                -- Connect to server side.
                bind sock $ SockAddrInet (fromIntegral $ sPortBase + ix) iNADDR_ANY
                -- Start listening for connections
                listen sock 1
                return sock

            (_, connsCmp) <- liftIO $ forkIO $ forM ssockss $ \ssock -> do
                (wconn, _) <- accept ssock
                (rconn, _) <- accept ssock
                return (wconn, rconn)

            middlePH <- command
                []
                javaCmd $
                [ "-jar", outputJarFile
                ] ++ middlewareArgs mwFlags

            bsconns <- liftIO $ connsCmp >>= result
            let sconns = map fst bsconns

            csock <- liftIO $ do
                let localhostAddr = tupleToHostAddress (127, 0, 0, 1)
                -- Create client socket
                sock <- socket AF_INET Stream defaultProtocol
                -- Make it immediately available
                setSocketOption sock ReuseAddr 1
                -- Connect to client side
                bind sock $ SockAddrInet (fromIntegral cPort) iNADDR_ANY
                connect sock $ SockAddrInet (fromIntegral mPort) localhostAddr
                return sock

            let timeoutTime = 1 * 1000 * 1000 -- One second
            let tryFailures :: ByteString -> ByteString -> Action ()
                tryFailures key dat = do
                    let req = "set " <> key <> " 0 0 " <> SB8.pack (show $ SB.length dat) <> "\r\n" <> dat <> "\r\n"

                    forM_ sixs $ \failingIx -> do
                        liftIO $ sendAll csock req
                        forM_ sixs $ \receivingIx -> do
                            let sconn = sconns !! receivingIx
                            mres <- liftIO $ timeout timeoutTime $ recv sconn 1024
                            case mres of
                                Nothing -> fail $ unwords
                                    [ "Failed to receive at server"
                                    , show receivingIx
                                    , "from middleware within"
                                    , show timeoutTime
                                    , "ns."
                                    ]
                                Just res -> unless (req == res) $ fail $ unlines
                                    [ "On input: " ++ show req
                                    , "the middleware sent " ++ show res ++ " to the server instead."
                                    ]
                        let errdat = case err of
                                NOT_STORED -> "NOT_STORED\r\n"
                                SERVER_ERROR -> "SERVER_ERROR here be an error\r\n"
                        forM_ sixs $ \sendingIx -> do
                            let sconn = sconns !! sendingIx
                            let output = if sendingIx == failingIx
                                    then errdat
                                    else "STORED\r\n"
                            liftIO $ sendAll sconn output
                        mres2 <- liftIO $ timeout timeoutTime $ recv csock 1024
                        case mres2 of
                            Nothing -> fail $ unwords
                                [ "Failed to receive at client from middleware within"
                                , show timeoutTime
                                , "ns."
                                ]
                            Just res2 -> unless (res2 == errdat) $ fail $ unlines
                                [ "On error: " ++ show errdat
                                , "the middleware sent " ++ show res2 ++ " to the client instead."
                                ]

            let tests = do
                    let testups = do
                            k <- ["a", "b", "c", "d"]
                            v <- ["abc", "bcd", "cde", "def"]
                            return (k, v)
                    forM_ testups $ uncurry tryFailures

            actionFinally tests $ do
                terminateProcess middlePH
                void $ waitForProcess middlePH
                forM_ bsconns $ \(rconn, wconn) -> do
                    close rconn
                    close wconn
                forM_ ssockss close
                close csock

