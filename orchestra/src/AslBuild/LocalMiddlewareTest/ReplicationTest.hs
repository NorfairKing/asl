{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AslBuild.LocalMiddlewareTest.ReplicationTest where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Data.Monoid
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString
import System.Process
import System.Timeout

import Development.Shake
import Development.Shake.FilePath

import AslBuild.CommonActions
import AslBuild.Constants
import AslBuild.Jar
import AslBuild.Memcached
import AslBuild.Middleware
import AslBuild.Types
import AslBuild.Utils

localMiddlewareReplicationTestRule :: String
localMiddlewareReplicationTestRule = "local-middleware-replication-test"

data ReplicationTestSetup = ReplicationTestSetup
    { cPortOffset :: Int
    , mPort :: Int
    , servers :: [MemcachedFlags]
    }

setups :: [ReplicationTestSetup]
setups = do
    nss <- takeWhile (<= 8) $ iterate (* 2) 1
    let ss =
            flip map [0 .. (nss - 1)] $ \ix ->
                MemcachedFlags {memcachedPort = 11262 + ix, memcachedAsDaemon = False}
    return ReplicationTestSetup {cPortOffset = 10000, mPort = 11271, servers = ss}

localMiddlewareReplicationTestRules :: Rules ()
localMiddlewareReplicationTestRules =
    phony localMiddlewareReplicationTestRule $
    forM_ setups $ \ReplicationTestSetup {..} -> do
        putLoud $
            "Running local middleware replication test with " ++
            show (length servers) ++ " servers."
        let mwFlags =
                MiddlewareFlags
                { mwIp = localhostIp
                , mwPort = mPort
                , mwNrThreads = 1
                , mwReplicationFactor = length servers
                , mwServers = map (RemoteServerUrl localhostIp . memcachedPort) servers
                , mwVerbosity = LogFine
                , mwTraceFile = tmpDir </> localMiddlewareReplicationTestRule ++ "-trace" <.> csvExt
                , mwReadSampleRate = Nothing
                , mwWriteSampleRate = Nothing
                }
        serverPhs <- forP servers runMemcachedLocally
        middlePH <- command [] javaCmd $ ["-jar", outputJarFile] ++ middlewareArgs mwFlags
        waitMs 250
        let localhostAddr = tupleToHostAddress (127, 0, 0, 1)
        let makeLocalSocket
                -- Create client socket
             = do
                sock <- socket AF_INET Stream defaultProtocol
                -- Make it immediately available
                setSocketOption sock ReuseAddr 1
                return sock
        cmwsock <-
            liftIO $ do
                sock <- makeLocalSocket
            -- Connect on client side to middleware
                bind sock $ SockAddrInet (fromIntegral cPortOffset) iNADDR_ANY
                connect sock $ SockAddrInet (fromIntegral mPort) localhostAddr
                return sock
        csocks <-
            liftIO $
            forM (zip [1 ..] servers) $ \(ix, MemcachedFlags {..}) -> do
                csock <- makeLocalSocket
            -- Connect on client side to each server
                bind csock $ SockAddrInet (fromIntegral cPortOffset + ix) iNADDR_ANY
                connect csock $ SockAddrInet (fromIntegral memcachedPort) localhostAddr
                return csock
        let shouldResultIn :: ByteString -> ByteString -> ByteString -> Action ()
            shouldResultIn setreq getreq output = do
                liftIO $ sendAll cmwsock setreq
                let timeouttime = 1 * 1000 * 1000 -- One second
                liftIO $ do
                    mres <- liftIO $ timeout timeouttime $ recv cmwsock 1024
                    case mres of
                        Nothing ->
                            fail $
                            unwords
                                [ "recv timed out after"
                                , show timeouttime
                                , "ns on middleware client"
                                ]
                        Just res ->
                            unless (res == "STORED\r\n") $
                            fail $
                            unlines
                                [ "On input: " ++ show setreq
                                , "Failed te store key, got this as response instead: " ++ show res
                                ]
                forM_ (indexed csocks) $ \(ix, csock) -> do
                    liftIO $ sendAll csock getreq
                    mres <- liftIO $ timeout timeouttime $ recv csock 1024
                    case mres of
                        Nothing ->
                            fail $
                            unwords
                                ["recv timed out after", show timeouttime, "ns on client", show ix]
                        Just res ->
                            unless (res == output) $
                            fail $
                            unlines
                                [ "On input: " ++ show getreq
                                , "Expected output: " ++ show output
                                , "But got: " ++ show res
                                , "On client: " ++ show ix
                                ]
        let replicationTest :: ByteString -> ByteString -> Action ()
            replicationTest key dat =
                shouldResultIn
                    ("set " <> key <> " 0 0 " <> SB8.pack (show $ SB.length dat) <> "\r\n" <> dat <>
                     "\r\n")
                    ("get " <> key <> "\r\n")
                    ("VALUE " <> key <> " 0 " <> SB8.pack (show $ SB.length dat) <> "\r\n" <> dat <>
                     "\r\n" <>
                     "END" <>
                     "\r\n")
        let tests = do
                let testups = do
                        k <- ["a", "b", "c", "d"]
                        d <- ["abc", "bcd", "cde", "def"]
                        return (k, d)
                forM_ testups $ uncurry replicationTest
        actionFinally tests $ do
            mapM_ terminateProcess serverPhs
            terminateProcess middlePH
            mapM_ waitForProcess serverPhs
            void $ waitForProcess middlePH
            close cmwsock
            forM_ csocks close
