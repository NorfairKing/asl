{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Types where

import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Csv
import           Data.Hashable
import           GHC.Generics

data Persistence
    = Persistent
    | Volatile
    deriving (Show, Eq, Generic)

instance ToJSON   Persistence
instance FromJSON Persistence

data Location
    = Local
    | Remote
    deriving (Show, Eq, Generic)

instance ToJSON   Location
instance FromJSON Location

data Script
    = Script
    { scriptName    :: FilePath
    , scriptContent :: [String]
    } deriving (Show, Eq)

namedScript :: FilePath -> [String] -> Script
namedScript name contents = Script name $ "#!/bin/bash" : contents

script :: [String] -> Script
script contents = namedScript (show $ hash contents) contents

data RemoteLogin
    = RemoteLogin
    { remoteUser :: Maybe String
    , remoteHost :: String
    } deriving (Show, Eq, Generic)

instance ToJSON   RemoteLogin
instance FromJSON RemoteLogin

remoteLoginStr :: RemoteLogin -> String
remoteLoginStr RemoteLogin{..} =
    case remoteUser of
        Nothing -> remoteHost
        Just user -> user ++ "@" ++ remoteHost


data RemoteServerUrl
    = RemoteServerUrl
    { serverUrl  :: String
    , serverPort :: Int
    } deriving (Show, Eq, Generic)

instance ToJSON   RemoteServerUrl
instance FromJSON RemoteServerUrl

remoteServerUrl :: RemoteServerUrl -> String
remoteServerUrl RemoteServerUrl{..} = serverUrl ++ ":" ++ show serverPort

loginToMemcachedServerUrl :: RemoteLogin -> RemoteServerUrl
loginToMemcachedServerUrl RemoteLogin{..} = RemoteServerUrl remoteHost 11211

data TimeUnit
    = Seconds Int
    | Minutes Int
    | Hours Int
    deriving (Show, Eq, Generic)

instance ToJSON   TimeUnit
instance FromJSON TimeUnit

instance ToField TimeUnit where
    toField = toField . toSeconds

toSeconds :: TimeUnit -> Int
toSeconds (Seconds i) = i
toSeconds (Minutes i) = 60 * i
toSeconds (Hours i)   = 60 * 60 * i

timeUnit :: TimeUnit -> String
timeUnit (Seconds i) = show i ++ "s"
timeUnit (Minutes i) = show i ++ "m"
timeUnit (Hours i)   = show i ++ "h"

