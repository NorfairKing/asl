{-# LANGUAGE RecordWildCards #-}

module AslBuild.Middleware
    ( module AslBuild.Middleware
    , module AslBuild.Middleware.Types
    ) where

import Development.Shake

import AslBuild.Constants
import AslBuild.Jar
import AslBuild.Middleware.Types
import AslBuild.Types
import AslBuild.Utils

runMiddlewareLocally
    :: CmdResult r
    => MiddlewareFlags -> Action r
runMiddlewareLocally flags = do
    need [outputJarFile]
    runMiddlewareLocally_ flags

runMiddlewareLocally_
    :: CmdResult r
    => MiddlewareFlags -> Action r
runMiddlewareLocally_ flags = cmd $ localMiddlewareCmd flags

localMiddlewareCmd :: MiddlewareFlags -> [String]
localMiddlewareCmd = middlewareCmds outputJarFile

middlewareCmds :: FilePath -> MiddlewareFlags -> [String]
middlewareCmds midPath flags = [javaCmd, "-jar", midPath] ++ middlewareArgs flags

middlewareArgs :: MiddlewareFlags -> [String]
middlewareArgs MiddlewareFlags {..} =
    [ "-l"
    , mwIp
    , "-p"
    , show mwPort
    , "-t"
    , show mwNrThreads
    , "-r"
    , show mwReplicationFactor
    , "-v"
    , show $ logLevelInt mwVerbosity
    , "-f"
    , mwTraceFile
    ] ++
    maybeFlag 'R' mwReadSampleRate ++
    maybeFlag 'W' mwWriteSampleRate ++ ["-m"] ++ map remoteServerUrl mwServers
