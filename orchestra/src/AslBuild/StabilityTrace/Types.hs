module AslBuild.StabilityTrace.Types where

import           AslBuild.Client.Types
import           AslBuild.Middle.Types
import           AslBuild.Middleware.Types
import           AslBuild.Server.Types
import           AslBuild.Types

data StabilityTraceCfg
    = StabilityTraceCfg
    { target     :: String
    , csvOutFile :: FilePath
    , nrClients  :: Int
    , nrServers  :: Int
    , location   :: StabilityLocation
    , runtime    :: TimeUnit
    , logLevel   :: LogLevel
    } deriving (Show, Eq)

data StabilityLocation
    = StabilityLocal
    | StabilityRemote
    deriving (Show, Eq)

data StabilityTraceSetup
    = StabilityTraceSetup
    { stsRuntime   :: TimeUnit
    , clientSetups :: [ClientSetup]
    , middleSetup  :: MiddleSetup
    , serverSetups :: [ServerSetup]
    } deriving (Show, Eq)
