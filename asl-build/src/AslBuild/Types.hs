{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module AslBuild.Types where

import           Data.Aeson
import           Data.Hashable
import           GHC.Generics

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
    { remoteUser :: String
    , remoteHost :: String
    } deriving (Show, Eq, Generic)

instance ToJSON   RemoteLogin
instance FromJSON RemoteLogin

remoteLoginStr :: RemoteLogin -> String
remoteLoginStr RemoteLogin{..} = remoteUser ++ "@" ++ remoteHost
