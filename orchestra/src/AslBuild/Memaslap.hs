{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Memaslap
    ( module AslBuild.Memaslap
    , module AslBuild.Memaslap.Types
    , module AslBuild.Memaslap.LogParser
    ) where

import           Data.List

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.Memaslap.LogParser
import           AslBuild.Memaslap.Types
import           AslBuild.Types

runMemaslapLocally :: CmdResult r => MemaslapFlags -> Action r
runMemaslapLocally flags = do
    need [memaslapBin]
    cmd $ memaslapCmds memaslapBin flags

memaslapCmds :: FilePath -> MemaslapFlags -> [String]
memaslapCmds path flags
    = path : memaslapArgs flags

memaslapArgs :: MemaslapFlags -> [String]
memaslapArgs MemaslapFlags{..} =
    [ "--servers=" ++ intercalate "," (map remoteServerUrl msServers)
    , "--threads=" ++ show msThreads
    , "--concurrency=" ++ show msConcurrency
    , "--overwrite=" ++ show msOverwrite
    , "--win_size=" ++ renderWindowSize msWindowSize
    , "--cfg_cmd=" ++ msConfigFile
    , case msWorkload of
        WorkFor msTime  -> "--time=" ++ timeUnit msTime
        NrRequests reqs -> "--execute_number=" ++ show reqs
    ]
    ++ case msStatFreq of
        Just statFreq -> ["--stat_freq=" ++ timeUnit statFreq]
        Nothing       -> []

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

singleDist :: Int -> [Distribution]
singleDist size = [Distribution size size 1]
