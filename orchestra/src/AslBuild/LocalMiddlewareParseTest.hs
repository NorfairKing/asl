{-# LANGUAGE OverloadedStrings #-}
module AslBuild.LocalMiddlewareParseTest where

import           Control.Monad
import           Data.ByteString           (ByteString)
import           Network.Socket            hiding (recv, send)
import           Network.Socket.ByteString
import           System.Process

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.Memcached
import           AslBuild.Middleware
import           AslBuild.Types

localMiddlewareParseTestRule :: String
localMiddlewareParseTestRule = "local-middleware-parse-test"

localMiddlewareParseTestRules :: Rules ()
localMiddlewareParseTestRules =
    localMiddlewareParseTestRule ~> do
        need [memcachedBin, memaslapBin, outputJarFile]

        let sPort :: Int
            sPort = 11211
        let memcachedFlags = MemcachedFlags
                { memcachedPort = sPort
                , memcachedAsDaemon = False
                }

        let clientPort :: Int
            clientPort = 11234
        let mwFlags = MiddlewareFlags
                { mwIp = "host"
                , mwPort = clientPort
                , mwNrThreads = 1
                , mwReplicationFactor = 1
                , mwServers = [RemoteServerUrl "localhost" sPort]
                }

        serverPH <- command [] memcachedBin
                (memcachedArgs memcachedFlags)

        middlePH <- command
            []
            javaCmd $
            [ "-jar", outputJarFile
            ] ++ middlewareArgs mwFlags


        wait 1

        csock <- liftIO $ do
            let localhostAddr = tupleToHostAddress (127, 0, 0, 1)
            -- Create client socket
            csock <- socket AF_INET Stream defaultProtocol
            -- Make it immediately available
            setSocketOption csock ReuseAddr 1
            -- Connect to client side
            bind csock (SockAddrInet 11235 iNADDR_ANY)
            connect csock $ SockAddrInet (fromIntegral clientPort) localhostAddr
            return csock

        let shouldResultIn :: ByteString -> ByteString -> Action ()
            shouldResultIn input output = do
                liftIO $ sendAll csock input
                res <- liftIO $ recv csock 1024
                unless (res == output) $ fail $ unlines
                    [ "On input: " ++ show input
                    , "Expected output: " ++ show output
                    , "But got: " ++ show res
                    ]

        let tests = do
                "get key\r\n" `shouldResultIn` "END\r\n"
                "get otherkey\r\n" `shouldResultIn` "END\r\n"
                "get moreKeys\r\n" `shouldResultIn` "END\r\n"
                "set key 0 0 8\r\n12345678\r\n" `shouldResultIn` "STORED\r\n"
                "get key\r\n" `shouldResultIn` "VALUE key 0 8\r\n12345678\r\nEND\r\n"
                "get otherkey\r\n" `shouldResultIn` "END\r\n"
                "set otherkey 0 0 3\r\nabc\r\n" `shouldResultIn` "STORED\r\n"
                "get otherkey\r\n" `shouldResultIn` "VALUE otherkey 0 3\r\nabc\r\nEND\r\n"
                "delete key\r\n" `shouldResultIn` "DELETED\r\n"
                "get key\r\n" `shouldResultIn` "END\r\n"
                "delete key\r\n" `shouldResultIn` "NOT_FOUND\r\n"
                "delete otherkey\r\n" `shouldResultIn` "DELETED\r\n"
                "get otherkey\r\n" `shouldResultIn` "END\r\n"
                "delete otherkey\r\n" `shouldResultIn` "NOT_FOUND\r\n"

        actionFinally tests $ do
            terminateProcess serverPH
            terminateProcess middlePH

