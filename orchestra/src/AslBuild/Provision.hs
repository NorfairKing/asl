{-# LANGUAGE RecordWildCards #-}
module AslBuild.Provision where

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memcached
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm
import           AslBuild.Vm.Types

provisionRules :: Rules ()
provisionRules = do
    provisionLocalhostRules
    provisionVmsRules

provisionLocalhostRule :: String
provisionLocalhostRule = "provision-localhost"

provisionLocalhostMemcachedRule :: String
provisionLocalhostMemcachedRule = "provision-localhost-memcached"

provisionLocalhostMemaslapRule :: String
provisionLocalhostMemaslapRule = "provision-localhost-memaslap"

provisionLocalhostRules :: Rules ()
provisionLocalhostRules = do
    provisionLocalhostRule ~> need
        [ provisionLocalhostMemcachedRule
        , provisionLocalhostMemaslapRule
        ]

    provisionLocalhostMemcachedRule ~>
        rsyncTo localhostLogin memcachedBin remoteMemcachedBin

    provisionLocalhostMemaslapRule ~>
        rsyncTo localhostLogin memaslapBin remoteMemaslapBin

localhostLogin :: RemoteLogin
localhostLogin = RemoteLogin Nothing "localhost"

provisionVmsRule :: String
provisionVmsRule = "provision-vms"

provisionVmsMemcachedRule :: String
provisionVmsMemcachedRule = "provision-vms-memcached"

provisionVmsMemaslapRule :: String
provisionVmsMemaslapRule = "provision-vms-memaslap"

provisionVmsRules :: Rules ()
provisionVmsRules = do
    provisionVmsRule ~> need
        [ provisionVmsMemcachedRule
        , provisionVmsMemaslapRule
        ]

    provisionVmsMemcachedRule ~>
        eachVm (\rl -> rsyncTo rl memcachedBin remoteMemcachedBin)

    provisionVmsMemaslapRule ~>
        eachVm (\rl -> rsyncTo rl memaslapBin remoteMemaslapBin)

eachVm :: (RemoteLogin -> Action ()) -> Action ()
eachVm func = do
    vms <- getVmsToProvision
    forP_ vms func

getVmsToProvision :: Action [RemoteLogin]
getVmsToProvision = do
    vms <- getRawVmData
    return $ map (\VmData{..} -> RemoteLogin (Just vmAdmin) vmFullUrl) vms

