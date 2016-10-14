{-# LANGUAGE RecordWildCards #-}
module AslBuild.Middleware
    ( module AslBuild.Middleware
    , module AslBuild.Middleware.Types
    ) where

import           AslBuild.Middleware.Types
import           AslBuild.Types

middlewareArgs :: MiddlewareFlags -> [String]
middlewareArgs MiddlewareFlags{..} =
    [ "-l", mwIp
    , "-p", show mwPort
    , "-t", show mwNrThreads
    , "-r", show mwReplicationFactor
    , "-v", show $ fromEnum mwVerbosity
    , "-f", mwTraceFile
    , "-m"
    ] ++ map remoteServerUrl mwServers
