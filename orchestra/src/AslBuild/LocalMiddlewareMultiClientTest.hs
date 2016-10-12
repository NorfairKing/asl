{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.LocalMiddlewareMultiClientTest where

import           Control.Monad
import           Data.ByteString           (ByteString)
import           Development.Shake
import           Network.Socket            hiding (recv, send)
import           Network.Socket.ByteString
import           System.Process
import           System.Timeout

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.Middleware
import           AslBuild.Types
import           AslBuild.Utils

-- TODO rename
localMiddlewareMultiClientTestRule :: String
localMiddlewareMultiClientTestRule = "local-middleware-multiclient-test"

data MulticlientTestSetup
    = MulticlientTestSetup
    { sPort       :: Int
    , mPort       :: Int
    , cPortOffset :: Int
    , nrClients   :: Int
    }

setups :: [MulticlientTestSetup]
setups = do
    ncs <- takeWhile (<= 8) $ iterate (*2) 1
    return MulticlientTestSetup
        { sPort = 11211
        , mPort = 11212
        , cPortOffset = 11213
        , nrClients = ncs
        }

-- TODO set nrClients to different values
localMiddlewareMultiClientTestRules :: Rules ()
localMiddlewareMultiClientTestRules =
    localMiddlewareMultiClientTestRule ~> do
        need [memcachedBin, memaslapBin, outputJarFile]

        forM_ setups $ \MulticlientTestSetup{..} -> do
            let memcachedFlags = MemcachedFlags
                    { memcachedPort = sPort
                    , memcachedAsDaemon = False
                    }

            let mwFlags = MiddlewareFlags
                    { mwIp = "localhost"
                    , mwPort = mPort
                    , mwNrThreads = 1
                    , mwReplicationFactor = 1
                    , mwServers = [RemoteServerUrl "localhost" sPort]
                    , mwVerbosity = LogAll
                    }

            serverPH <- command [] memcachedBin
                    (memcachedArgs memcachedFlags)

            middlePH <- command
                []
                javaCmd $
                [ "-jar", outputJarFile
                ] ++ middlewareArgs mwFlags


            waitMs 250

            csocks <- liftIO $ forM [1 .. nrClients] $ \i -> do
                let localhostAddr = tupleToHostAddress (127, 0, 0, 1)
                -- Create client socket
                csock <- socket AF_INET Stream defaultProtocol
                -- Make it immediately available
                setSocketOption csock ReuseAddr 1
                -- Connect to client side
                bind csock (SockAddrInet (fromIntegral $ cPortOffset + i) iNADDR_ANY)
                connect csock $ SockAddrInet (fromIntegral mPort) localhostAddr
                return csock

            let shouldResultIn :: ByteString -> ByteString -> Action ()
                shouldResultIn input output = do
                    forM_ csocks $ \csock -> liftIO $ sendAll csock input
                    forM_ (indexed csocks) $ \(ix, csock) -> do
                        let timeouttime = 1 * 1000 * 1000 -- One second
                        mres <- liftIO $ timeout timeouttime $ recv csock 1024
                        case mres of
                            Nothing -> fail $ unwords
                                [ "recv timed out after"
                                , show timeouttime
                                , "ns on client"
                                , show ix
                                ]
                            Just res ->
                                unless (res == output) $ fail $ unlines
                                    [ "On input: " ++ show input
                                    , "Expected output: " ++ show output
                                    , "But got: " ++ show res
                                    , "On client: " ++ show ix
                                    ]

            let tests = do
                    -- Successful requests

                    -- Get keys that don't have data assigned.
                    "get key\r\n" `shouldResultIn` "END\r\n"
                    "get otherkey\r\n" `shouldResultIn` "END\r\n"
                    "get moreKeys\r\n" `shouldResultIn` "END\r\n"

                    -- Set data for 'key'
                    "set key 0 0 8\r\n12345678\r\n" `shouldResultIn` "STORED\r\n"

                    -- Get it back
                    "get key\r\n" `shouldResultIn` "VALUE key 0 8\r\n12345678\r\nEND\r\n"

                    -- Check that getting a nonexistent piece still works
                    "get otherkey\r\n" `shouldResultIn` "END\r\n"

                    -- Do the same thing as for 'key', but for 'otherkey'.
                    "set otherkey 0 0 3\r\nabc\r\n" `shouldResultIn` "STORED\r\n"
                    "get otherkey\r\n" `shouldResultIn` "VALUE otherkey 0 3\r\nabc\r\nEND\r\n"

                    -- Check for client_error on something that doesnt conform to the protocol,
                    -- like a missing newline
                    "g" `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"
                    "ge" `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"
                    "get" `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"
                    "get " `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"
                    "get k" `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"
                    "get ke" `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"
                    "get key" `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"
                    "get key\r" `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"

                    -- Check for error on nonexistent command
                    "ste key\r\n" `shouldResultIn` "ERROR\r\n"
                    "deltee key\r\n" `shouldResultIn` "ERROR\r\n"
                    "a\r\n" `shouldResultIn` "ERROR\r\n"
                    "aaa" `shouldResultIn` "ERROR\r\n"


            actionFinally tests $ do
                terminateProcess serverPH
                terminateProcess middlePH
                void $ waitForProcess serverPH
                void $ waitForProcess middlePH
                forM_ csocks close

