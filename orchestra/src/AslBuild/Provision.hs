{-# LANGUAGE RecordWildCards #-}
module AslBuild.Provision where

import           Control.Monad
import           System.Process

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.PreCommit
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

provisionLocalhostGlobalPackagesRule :: String
provisionLocalhostGlobalPackagesRule = "provision-localhost-global-packages"

provisionLocalhostOrcRule :: String
provisionLocalhostOrcRule = "provision-localhost-orc"

provisionLocalhostMemcachedRule :: String
provisionLocalhostMemcachedRule = "provision-localhost-memcached"

provisionLocalhostMemaslapRule :: String
provisionLocalhostMemaslapRule = "provision-localhost-memaslap"

provisionLocalhostRules :: Rules ()
provisionLocalhostRules = do
    provisionLocalhostRule ~> do
        need [provisionLocalhostGlobalPackagesRule]
        need [provisionLocalhostOrcRule]
        need
            [ provisionLocalhostMemcachedRule
            , provisionLocalhostMemaslapRule
            ]

    provisionLocalhostGlobalPackagesRule ~> return ()

    orcBin `byCopying` buildBinInStack

    provisionLocalhostOrcRule ~> need [orcBin]

    provisionLocalhostMemcachedRule ~> need [memcachedBin]

    provisionLocalhostMemaslapRule ~> need [memaslapBin]

localhostLogin :: RemoteLogin
localhostLogin = RemoteLogin Nothing "localhost"

provisionVmsGlobalPackagesRule :: String
provisionVmsGlobalPackagesRule = "provision-vms-global-packages"

provisionVmsOrcRule :: String
provisionVmsOrcRule = "provision-vms-orc"

provisionVmsMemcachedRule :: String
provisionVmsMemcachedRule = "provision-vms-memcached"

provisionVmsMemaslapRule :: String
provisionVmsMemaslapRule = "provision-vms-memaslap"

provisionVmsRules :: Rules ()
provisionVmsRules = do
    provisionVmsGlobalPackagesRule ~> do
        eachVm $ \rl -> overSsh rl "yes | sudo apt-get update"
        eachVm $ \rl -> overSsh rl "yes | sudo apt-get install build-essential htop"

    provisionVmsOrcRule ~> do
        need [orcBin]
        eachVm' (\rl -> rsyncTo rl orcBin orcBin)

    provisionVmsMemcachedRule ~>
        eachVm (`orcRemotely` memcachedBin)

    provisionVmsMemaslapRule ~>
        eachVm (`orcRemotely` memaslapBin)

orcRemotely :: CmdResult r => RemoteLogin -> String -> Action r
orcRemotely rl target = overSsh rl $ unwords [orcBin, "build", target]

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

