{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module AslBuild.Vm.Types where

import           Data.Aeson
import           GHC.Generics

data VmData
    = VmData
    { vmName      :: String
    , vmPublicIp  :: String
    , vmPrivateIp :: String
    , vmFullUrl   :: String
    } deriving (Show, Eq, Generic)

instance ToJSON VmData
instance FromJSON VmData

newtype VmDataInAzureFormat = VmDataInAzureFormat { azureUnpack :: VmData }

instance FromJSON VmDataInAzureFormat where
    parseJSON (Object o) = do
        name <- o .: "name"
        (Object np) <- o .: "networkProfile"
        [Object ni] <- np .: "networkInterfaces"
        (Object nie) <- ni .: "expanded"
        [Object ipv] <- nie .: "ipConfigurations"
        privip <- ipv .: "privateIPAddress"
        pip <- ipv .: "publicIPAddress"
        pipe <- pip .: "expanded"
        publip <- pipe .: "ipAddress"
        (Object dns) <- pipe .: "dnsSettings"
        fullUrl <- dns .: "fqdn"

        return $ VmDataInAzureFormat VmData
            { vmName = name
            , vmPublicIp = publip
            , vmPrivateIp = privip
            , vmFullUrl = fullUrl
            }

    parseJSON _ = mempty
