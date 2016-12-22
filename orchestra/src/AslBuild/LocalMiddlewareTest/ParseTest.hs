{-# LANGUAGE OverloadedStrings #-}

module AslBuild.LocalMiddlewareTest.ParseTest where

import Control.Monad
import Data.ByteString (ByteString)
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import System.Process
import System.Timeout

import Control.Concurrent.Thread

import Development.Shake
import Development.Shake.FilePath

import AslBuild.Constants
import AslBuild.Middleware
import AslBuild.Provision
import AslBuild.Types

localMiddlewareParseTestRule :: String
localMiddlewareParseTestRule = "local-middleware-parse-test"

localMiddlewareParseTestRules :: Rules ()
localMiddlewareParseTestRules =
    localMiddlewareParseTestRule ~> do
        need [provisionLocalhostRule]
        let cPort :: Int
            cPort = 11234
        let mPort :: Int
            mPort = 11235
        let sPort :: Int
            sPort = 11236
        let mwFlags =
                MiddlewareFlags
                { mwIp = localhostIp
                , mwPort = mPort
                , mwNrThreads = 1
                , mwReplicationFactor = 1
                , mwServers = [RemoteServerUrl localhostIp sPort]
                , mwVerbosity = LogAll
                , mwTraceFile = tmpDir </> localMiddlewareParseTestRule ++ "-trace" <.> csvExt
                , mwReadSampleRate = Nothing
                , mwWriteSampleRate = Nothing
                }
        ssock <-
            liftIO $
            -- Create server socket
             do
                sock <- socket AF_INET Stream defaultProtocol
            -- Make it immediately available
                setSocketOption sock ReuseAddr 1
            -- Connect to server side.
                bind sock $ SockAddrInet (fromIntegral sPort) iNADDR_ANY
            -- Start listening for connections
                listen sock 1
                return sock
        (_, connCmp) <-
            liftIO $
            forkIO $ do
                (wconn, _) <- accept ssock
                (rconn, _) <- accept ssock
                return (wconn, rconn)
        middlePH <- runMiddlewareLocally mwFlags
        (wconn, rconn) <- liftIO $ connCmp >>= result
        csock <-
            liftIO $ do
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
        let shouldResultIn :: RequestKind -> ByteString -> ByteString -> Action ()
            shouldResultIn reqKind input output = do
                let sconn =
                        case reqKind of
                            READ -> rconn
                            WRITE -> wconn
                liftIO $ sendAll csock input
                mres <- liftIO $ timeout timeoutTime $ recv sconn 1024
                case mres of
                    Nothing ->
                        fail $
                        unwords
                            [ "Failed to receive at server from middleware within"
                            , show timeoutTime
                            , "ns."
                            ]
                    Just res ->
                        unless (input == res) $
                        fail $
                        unlines
                            [ "On input: " ++ show input
                            , "the middleware sent " ++ show res ++ " to the server instead."
                            ]
                liftIO $ sendAll sconn output
                mres2 <- liftIO $ timeout timeoutTime $ recv csock 1024
                case mres2 of
                    Nothing ->
                        fail $
                        unwords
                            [ "Failed to receive at client from middleware within"
                            , show timeoutTime
                            , "ns."
                            ]
                    Just res2 ->
                        unless (output == res2) $
                        fail $
                        unlines
                            [ "On output: " ++ show output
                            , "the middleware sent " ++ show res2 ++ " to the client instead."
                            ]
        let shouldErrorWith :: ByteString -> ByteString -> Action ()
            shouldErrorWith input output = do
                liftIO $ sendAll csock input
                mres <- liftIO $ timeout timeoutTime $ recv csock 1024
                case mres of
                    Nothing ->
                        fail $
                        unwords
                            [ "Failed to receive at server from middleware within"
                            , show timeoutTime
                            , "ns."
                            ]
                    Just res ->
                        unless (output == res) $
                        fail $
                        unlines
                            [ "On input: " ++ show input
                            , "the middleware responded " ++ show res
                            , "instead of the expected error: " ++ show output
                            ]
        let tests
                -- Successful requests
                -- Get keys that don't have data assigned.
             = do
                shouldResultIn READ "get key\r\n" "END\r\n"
                shouldResultIn READ "get otherkey\r\n" "END\r\n"
                shouldResultIn READ "get moreKeys\r\n" "END\r\n"
                -- Set data for 'key'
                shouldResultIn WRITE "set key 0 0 8\r\n12345678\r\n" "STORED\r\n"
                -- Get it back
                shouldResultIn READ "get key\r\n" "VALUE key 0 8\r\n12345678\r\nEND\r\n"
                -- Check that getting a nonexistent piece still works
                shouldResultIn READ "get otherkey\r\n" "END\r\n"
                -- Do the same thing as for 'key', but for 'otherkey'.
                shouldResultIn WRITE "set otherkey 0 0 3\r\nabc\r\n" "STORED\r\n"
                shouldResultIn READ "get otherkey\r\n" "VALUE otherkey 0 3\r\nabc\r\nEND\r\n"
                -- Delete the value for 'key'.
                shouldResultIn WRITE "delete key\r\n" "DELETED\r\n"
                -- Check that its data is indeed gone now.
                shouldResultIn READ "get key\r\n" "END\r\n"
                -- Check what happens if the data was already gone.
                shouldResultIn WRITE "delete key\r\n" "NOT_FOUND\r\n"
                -- Do the same thing for 'otherkey'.
                shouldResultIn WRITE "delete otherkey\r\n" "DELETED\r\n"
                shouldResultIn READ "get otherkey\r\n" "END\r\n"
                shouldResultIn WRITE "delete otherkey\r\n" "NOT_FOUND\r\n"
                -- Check for error on nonexistent command
                shouldErrorWith "st" "ERROR\r\n"
                shouldErrorWith "ste key\r\n" "ERROR\r\n"
                shouldErrorWith "aaa\r\n" "ERROR\r\n"
                shouldErrorWith "deltee k\r\n" "ERROR\r\n"
                shouldErrorWith "aaa\r\n" "ERROR\r\n"
        actionFinally tests $ do
            terminateProcess middlePH
            void $ waitForProcess middlePH
            close rconn
            close wconn
            close ssock
            close csock
