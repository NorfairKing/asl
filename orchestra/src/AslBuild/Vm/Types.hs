{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module AslBuild.Vm.Types
    ( VmData(..)
    , AzureVmData(..)
    ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Maybe
import qualified Data.Vector      as V
import           GHC.Generics

data VmData
    = VmData
    { vmName      :: String
    , vmPublicIp  :: String
    , vmPrivateIp :: String
    , vmAdmin     :: String
    , vmFullUrl   :: String
    , vmType      :: String
    } deriving (Show, Eq, Generic)

instance ToJSON VmData
instance FromJSON VmData

newtype AzureVmData = AzureVmData { azureUnpack :: [VmData] }

instance FromJSON AzureVmData where
    parseJSON v = aParser v
      where
        aParser = withArray "AzureVmData" $ \a -> do
            vms <- forM a $ \iv -> do
                let mvm = parseMaybe (withObject "SingleVmData" parseSingleVm) iv
                return mvm
            return $ AzureVmData $ catMaybes $ V.toList vms

parseSingleVm :: Object -> Parser VmData
parseSingleVm o = do
    name <- o .: "name"
    (Object osp) <- o .: "osProfile"
    (Object hp) <- o .: "hardwareProfile"
    tp <- hp .: "vmSize"
    admin <- osp .: "adminUsername"
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

    return VmData
        { vmName = name
        , vmPublicIp = publip
        , vmPrivateIp = privip
        , vmAdmin = admin
        , vmFullUrl = fullUrl
        , vmType = tp
        }
