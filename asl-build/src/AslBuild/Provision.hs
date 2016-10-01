{-# LANGUAGE RecordWildCards #-}
module AslBuild.Provision where

import           Development.Shake

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Memcached
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm
import           AslBuild.Vm.Types

provisionRule :: String
provisionRule = "provision-vms"

provisionMemcachedRule :: String
provisionMemcachedRule = "provision-memcached"

provisionMemaslapRule :: String
provisionMemaslapRule = "provision-memaslap"

provisionRules :: Rules ()
provisionRules = do
    provisionRule ~> need
        [ provisionMemcachedRule
        , provisionMemaslapRule
        ]

    provisionMemcachedRule ~>
        eachVm (\rl -> rsyncTo rl memcachedBin remoteMemcachedBin)

    provisionMemaslapRule ~>
        eachVm (\rl -> rsyncTo rl memaslapBin remoteMemaslapBin)

eachVm :: (RemoteLogin -> Action ()) -> Action ()
eachVm func = do
    vms <- getVmsToProvision
    forP_ vms func

getVmsToProvision :: Action [RemoteLogin]
getVmsToProvision = do
    vms <- getRawVmData
    return $
        RemoteLogin Nothing "localhost"
        : map (\VmData{..} -> RemoteLogin (Just vmAdmin) vmFullUrl) vms
