{-# LANGUAGE RecordWildCards #-}
module AslBuild.Provision where

import           Control.Monad
import           System.Process

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Types
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
        need [memcachedBin]

    provisionLocalhostMemaslapRule ~>
        need [memaslapBin]

localhostLogin :: RemoteLogin
localhostLogin = RemoteLogin Nothing "localhost"

provisionVmsRule :: String
provisionVmsRule = "provision-vms"

provisionVmsGlobalPackagesRule :: String
provisionVmsGlobalPackagesRule = "provision-vms-global-packages"

provisionOrcRule :: String
provisionOrcRule = "provision-vms-orc"

provisionVmsMemcachedRule :: String
provisionVmsMemcachedRule = "provision-vms-memcached"

provisionVmsMemaslapRule :: String
provisionVmsMemaslapRule = "provision-vms-memaslap"

provisionVmsRules :: Rules ()
provisionVmsRules = do
    provisionVmsRule ~> need
        [ provisionVmsGlobalPackagesRule
        , provisionOrcRule
        , provisionVmsMemcachedRule
        , provisionVmsMemaslapRule
        ]

    provisionVmsGlobalPackagesRule ~> do
        eachVm $ \rl -> overSsh rl "yes | sudo apt-get update"
        eachVm $ \rl -> overSsh rl "yes | sudo apt-get install build-essential htop"

    provisionOrcRule ~>
        eachVm' (\rl -> rsyncTo rl "/home/syd/.local/bin/orc" "orc")

    provisionVmsMemcachedRule ~>
        eachVm (`orcRemotely` "out/memcached")

    provisionVmsMemaslapRule ~>
        eachVm (`orcRemotely` "out/memaslap")

orcRemotely :: CmdResult r => RemoteLogin -> String -> Action r
orcRemotely rl target = overSsh rl $ "./orc build " ++ target

eachVm :: (RemoteLogin -> Action ProcessHandle) -> Action ()
eachVm func = do
    vms <- getVmsToProvision
    phs <- forM vms func
    liftIO $ mapM_ waitForProcess phs

eachVm' :: (RemoteLogin -> Action ()) -> Action ()
eachVm' func = do
    vms <- getVmsToProvision
    forM_ vms func

getVmsToProvision :: Action [RemoteLogin]
getVmsToProvision = do
    vms <- getRawVmData
    return $ map (\VmData{..} -> RemoteLogin (Just vmAdmin) vmFullUrl) vms

