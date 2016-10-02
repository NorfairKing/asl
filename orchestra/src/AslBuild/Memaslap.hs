{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Memaslap
    ( module AslBuild.Memaslap
    , module AslBuild.Memaslap.Types
    ) where

import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.Csv
import           Data.List
import           GHC.Generics

import           AslBuild.Memaslap.Types
import           AslBuild.Types

memaslapArgs :: MemaslapFlags -> [String]
memaslapArgs MemaslapFlags{..} =
    [ "--servers=" ++ intercalate "," (map remoteServerUrl msServers)
    , "--threads=" ++ show msThreads
    , "--concurrency=" ++ show msConcurrency
    , "--overwrite=" ++ show msOverwrite
    , "--stat_freq=" ++ timeUnit msStatFreq
    , "--time=" ++ timeUnit msTime
    , "--cfg_cmd=" ++ msConfigFile
    ]

renderMemaslapConfig :: MemaslapConfig -> String
renderMemaslapConfig MemaslapConfig{..} =
    unlines $
    [ "key"
    ] ++ map renderDistribution keysizeDistributions ++
    [ "value"
    ] ++ map renderDistribution valueDistributions ++
    [ "cmd"
    , unwords ["0", show setProportion]
    , unwords ["1", show getProportion]
    ]

renderDistribution :: Distribution -> String
renderDistribution Distribution{..} = unwords
    [ show distrMin, show distrMax, show distrProp ]

parseLog :: String -> Maybe MemaslapLog
parseLog s = do
        -- Required because there are also set statistics.
    let skippedFirsts = dropWhile (\l -> not $ "Total Statistics" `isPrefixOf` l) $ lines s
        avgPrefix = "   Avg:"
        stdPrefix = "   Std:"
    let getDoubleFromPrefix prefix
            = (read . drop (length prefix)) <$> find (\l -> prefix `isPrefixOf` l) skippedFirsts
    expavg <- getDoubleFromPrefix avgPrefix
    expstd <- getDoubleFromPrefix stdPrefix
    let getTps = (read . (!! 6) . words) <$> find (\l -> "Run time" `isPrefixOf` l) skippedFirsts
    exptps <- getTps
    return MemaslapLog
        { avg = expavg
        , std = expstd
        , tps = exptps
        }

memaslapLogsCsv :: [MemaslapLog] -> String
memaslapLogsCsv = LB8.unpack . encodeByName (header ["avg", "std", "tps"])


