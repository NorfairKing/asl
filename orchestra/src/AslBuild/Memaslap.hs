{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Memaslap
    ( module AslBuild.Memaslap
    , module AslBuild.Memaslap.Types
    , module AslBuild.Memaslap.LogParser
    ) where

import           Data.List

import           AslBuild.Memaslap.LogParser
import           AslBuild.Memaslap.Types
import           AslBuild.Types

memaslapArgs :: MemaslapFlags -> [String]
memaslapArgs MemaslapFlags{..} =
    [ "--servers=" ++ intercalate "," (map remoteServerUrl msServers)
    , "--threads=" ++ show msThreads
    , "--concurrency=" ++ show msConcurrency
    , "--overwrite=" ++ show msOverwrite
    , case msStatFreq of
        Just statFreq -> "--stat_freq=" ++ timeUnit statFreq
        Nothing       -> ""
    , case msWorkload of
        WorkFor msTime  -> "--time=" ++ timeUnit msTime
        NrRequests reqs -> "--execute_number=" ++ show reqs
    , "--cfg_cmd=" ++ msConfigFile
    ]

msTimeUnsafe :: MemaslapWorkload -> TimeUnit
msTimeUnsafe msWorkload = case msWorkload of
    WorkFor msTime -> msTime
    w -> error $ "wrong kind of workload, should be WorkFor instead of " ++ show w

renderMemaslapConfig :: MemaslapConfig -> String
renderMemaslapConfig MemaslapConfig{..} =
    unlines $
    [ "key"
    ] ++ map renderDistribution keysizeDistributions ++
    [ "value"
    ] ++ map renderDistribution valueDistributions ++
    [ "cmd"
    , unwords ["0", show setProportion]
    , unwords ["1", show $ 1 - setProportion]
    ]

renderDistribution :: Distribution -> String
renderDistribution Distribution{..} = unwords
    [ show distrMin, show distrMax, show distrProp ]

defaultMemaslapConfig :: MemaslapConfig
defaultMemaslapConfig = MemaslapConfig
    { keysizeDistributions = [Distribution 16 16 1]
    , valueDistributions = [Distribution 128 128 1]
    , setProportion = 0.01
    }
