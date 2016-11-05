{-# LANGUAGE RecordWildCards #-}
module AslBuild.Middleware
    ( module AslBuild.Middleware
    , module AslBuild.Middleware.Types
    ) where

import           AslBuild.Middleware.Types
import           AslBuild.Types
import           AslBuild.Utils

middlewareArgs :: MiddlewareFlags -> [String]
middlewareArgs MiddlewareFlags{..} =
    [ "-l", mwIp
    , "-p", show mwPort
    , "-t", show mwNrThreads
    , "-r", show mwReplicationFactor
    , "-v", show $ logLevelInt mwVerbosity
    , "-f", mwTraceFile
    ] ++ maybeFlag 'R' mwReadSampleRate
      ++ maybeFlag 'W' mwWriteSampleRate
      ++
    [ "-m"
    ] ++ map remoteServerUrl mwServers

