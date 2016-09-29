{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Memaslap where

import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.Csv
import           Data.List
import           GHC.Generics

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

data MemaslapSettings
    = MemaslapSettings
    { msConfig :: MemaslapConfig
    , msFlags  :: MemaslapFlags
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapSettings
instance FromJSON MemaslapSettings

data MemaslapConfig
    = MemaslapConfig
    { keysizeDistributions :: [Distribution]
    , valueDistributions   :: [Distribution]
    , setProportion        :: Double
    , getProportion        :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapConfig
instance FromJSON MemaslapConfig

data Distribution
    = Distribution
    { distrMin  :: Int
    , distrMax  :: Int
    , distrProp :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON   Distribution
instance FromJSON Distribution

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

data MemaslapFlags
    = MemaslapFlags
    { msServers     :: [RemoteServerUrl]
    , msThreads     :: Int
    , msConcurrency :: Int
    , msOverwrite   :: Double
    , msStatFreq    :: TimeUnit
    , msTime        :: TimeUnit
    , msConfigFile  :: FilePath
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapFlags
instance FromJSON MemaslapFlags

data TimeUnit
    = Seconds Int
    deriving (Show, Eq, Generic)

instance ToJSON   TimeUnit
instance FromJSON TimeUnit

instance ToField TimeUnit where
    toField = toField . toSeconds

toSeconds :: TimeUnit -> Int
toSeconds (Seconds i) = i

timeUnit :: TimeUnit -> String
timeUnit (Seconds i) = show i ++ "s"

data MemaslapLog
    = MemaslapLog
    { avg :: Double
    , std :: Double
    , tps :: Double
    } deriving (Show, Eq, Generic)

instance ToJSON   MemaslapLog
instance FromJSON MemaslapLog

instance ToNamedRecord MemaslapLog where
    toNamedRecord MemaslapLog{..} = namedRecord
        [ "avg" .= avg, "std" .= std, "tps" .= tps]

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


