{-# LANGUAGE OverloadedStrings #-}
module AslBuild.LocalMiddlewareMultiClientTest where

import           Control.Monad
import           Data.ByteString           (ByteString)
import           Network.Socket            hiding (recv, send)
import           Network.Socket.ByteString
import           System.Process
import           System.Timeout

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.Middleware
import           AslBuild.Types

-- TODO rename
localMiddlewareMultiClientTestRule :: String
localMiddlewareMultiClientTestRule = "local-middleware-multiclient-test"

-- TODO set nrClients to different values
localMiddlewareMultiClientTestRules :: Rules ()
localMiddlewareMultiClientTestRules =
    localMiddlewareMultiClientTestRule ~> do
        need [memcachedBin, memaslapBin, outputJarFile]

        let sPort :: Int
            sPort = 12344
        let memcachedFlags = MemcachedFlags
                { memcachedPort = sPort
                , memcachedAsDaemon = False
                }

        let mPort :: Int
            mPort = 12345

        let mwFlags = MiddlewareFlags
                { mwIp = "host"
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


        wait 1

        let nrClients = 1 -- TODO multiple clients
        csocks <- liftIO $ forM [1..nrClients] $ \i -> do
            let localhostAddr = tupleToHostAddress (127, 0, 0, 1)
            -- Create client socket
            csock <- socket AF_INET Stream defaultProtocol
            -- Make it immediately available
            setSocketOption csock ReuseAddr 1
            -- Connect to client side
            bind csock (SockAddrInet (12346 + i) iNADDR_ANY)
            connect csock $ SockAddrInet (fromIntegral mPort) localhostAddr
            return csock

        let shouldResultIn :: ByteString -> ByteString -> Action ()
            shouldResultIn input output = do
                forM_ csocks $ \csock -> liftIO $ sendAll csock input
                forM_ csocks $ \csock -> do
                    let timeouttime = 1 * 1000 * 1000 -- One second
                    mres <- liftIO $ timeout timeouttime $ recv csock 1024
                    case mres of
                        Nothing -> fail $ unwords
                            [ "recv timed out after"
                            , show timeouttime
                            , "ns."
                            ]
                        Just res ->
                            unless (res == output) $ fail $ unlines
                                [ "On input: " ++ show input
                                , "Expected output: " ++ show output
                                , "But got: " ++ show res
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

                -- Delete the value for 'key'.
                "delete key\r\n" `shouldResultIn` "DELETED\r\n"

                -- Check that its data is indeed gone now.
                "get key\r\n" `shouldResultIn` "END\r\n"

                -- Check what happens if the data was already gone.
                "delete key\r\n" `shouldResultIn` "NOT_FOUND\r\n"

                -- Do the same thing for 'otherkey'.
                "delete otherkey\r\n" `shouldResultIn` "DELETED\r\n"
                "get otherkey\r\n" `shouldResultIn` "END\r\n"
                "delete otherkey\r\n" `shouldResultIn` "NOT_FOUND\r\n"

                -- Check for error on nonexistent command
                "ste key\r\n" `shouldResultIn` "ERROR\r\n"

                -- Check for client_error on something that doesnt conform to the protocol,
                -- like a missing newline
                "get key\r" `shouldResultIn` "CLIENT_ERROR Not enough data.\r\n"

                liftIO $ terminateProcess serverPH

                "get key\r\n" `shouldResultIn`
                    "SERVER_ERROR 0 bytes read from server: localhost/127.0.0.1:12344\r\n"
                "set key 0 0 8\r\n12345678\r\n" `shouldResultIn`
                    "SERVER_ERROR 0 bytes read from server: localhost/127.0.0.1:12344\r\n"
                "delete key\r\n" `shouldResultIn`
                    "SERVER_ERROR Failed to write to server: localhost/127.0.0.1:12344\r\n"

        actionFinally tests $ do
            terminateProcess serverPH
            terminateProcess middlePH

