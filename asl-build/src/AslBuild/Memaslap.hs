{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Memaslap where

import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.Csv
import           Data.List


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

data MemaslapFlags
    = MemaslapFlags
    { msServers     :: [RemoteServerUrl]
    , msThreads     :: Int
    , msConcurrency :: Int
    , msOverwrite   :: Double
    , msStatFreq    :: TimeUnit
    , msTime        :: TimeUnit
    , msConfigFile  :: FilePath
    }

data RemoteServerUrl
    = RemoteServerUrl
    { serverUrl  :: String
    , serverPort :: Int
    }

remoteServerUrl :: RemoteServerUrl -> String
remoteServerUrl RemoteServerUrl{..} = serverUrl ++ ":" ++ show serverPort

data TimeUnit
    = Seconds Int

timeUnit :: TimeUnit -> String
timeUnit (Seconds i) = show i ++ "s"

data ParsedLog
    = ParsedLog
    { avg :: Double
    , std :: Double
    , tps :: Double
    } deriving (Show, Eq)

instance ToNamedRecord ParsedLog where
    toNamedRecord ParsedLog{..} = namedRecord
        [ "avg" .= avg, "std" .= std, "tps" .= tps]

parseLog :: String -> Maybe ParsedLog
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
    return ParsedLog
        { avg = expavg
        , std = expstd
        , tps = exptps
        }

parsedLogsCsv :: [ParsedLog] -> String
parsedLogsCsv = LB8.unpack . encodeByName (header ["avg", "std", "tps"])
