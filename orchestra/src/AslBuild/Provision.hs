{-# LANGUAGE RecordWildCards #-}
module AslBuild.Provision where

import           Control.Monad
import           Data.List
import           System.FilePath
import           System.Process

import           Development.Shake

import           AslBuild.BuildMemcached
import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Jar
import           AslBuild.Orc
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm

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

provisionLocalhostMiddlewareRule :: String
provisionLocalhostMiddlewareRule = "provision-localhost-middleware"

provisionLocalhostRules :: Rules ()
provisionLocalhostRules = do
    provisionLocalhostRule ~> do
        need [provisionLocalhostGlobalPackagesRule]
        need [provisionLocalhostOrcRule]
        need
            [ provisionLocalhostMemcachedRule
            , provisionLocalhostMemaslapRule
            , provisionLocalhostMiddlewareRule
            ]

    provisionLocalhostGlobalPackagesRule ~> return ()
    provisionLocalhostOrcRule ~> need [orcBin]

    remoteMemcached `byCopying` memcachedBin
    provisionLocalhostMemcachedRule ~> need [remoteMemcached]

    remoteMemaslap `byCopying` memaslapBin
    provisionLocalhostMemaslapRule ~> need [remoteMemaslap]

    remoteMiddleware `byCopying` outputJarFile
    provisionLocalhostMiddlewareRule ~> need [remoteMiddleware]

localhostLogin :: RemoteLogin
localhostLogin = RemoteLogin Nothing "localhost"

provisionVmsRule :: String
provisionVmsRule = "provision-vms"

provisionVmsGlobalPackagesRule :: String
provisionVmsGlobalPackagesRule = "provision-vms-global-packages"

provisionVmsOrcRule :: String
provisionVmsOrcRule = "provision-vms-orc"

provisionVmsMemcachedRule :: String
provisionVmsMemcachedRule = "provision-vms-memcached"

provisionVmsMemaslapRule :: String
provisionVmsMemaslapRule = "provision-vms-memaslap"

provisionVmsMiddlewareRule :: String
provisionVmsMiddlewareRule = "provision-vms-middleware"

provisionVmsRules :: Rules ()
provisionVmsRules = do
    provisionVmsRule ~> (getAllVmsToProvision >>= provisionVms)
    provisionVmsGlobalPackagesRule ~> (getAllVmsToProvision >>= provisionVmsGlobalPackages)
    provisionVmsOrcRule ~> (getAllVmsToProvision >>= provisionVmsOrc)
    provisionVmsMemcachedRule ~> (getAllVmsToProvision >>= provisionVmsMemcached)
    provisionVmsMemaslapRule ~> (getAllVmsToProvision >>= provisionVmsMemaslap)
    provisionVmsMiddlewareRule ~> (getAllVmsToProvision >>= provisionVmsMiddleware)

provisionVmsFromData :: [VmData] -> Action ()
provisionVmsFromData = provisionVms . nub . map (\VmData{..} -> RemoteLogin (Just vmAdmin) vmPublicIp)

provisionVms :: [RemoteLogin] -> Action ()
provisionVms rls = do
    provisionVmsGlobalPackages rls
    provisionVmsOrc rls
    provisionVmsMemcached rls
    provisionVmsMemaslap rls
    provisionVmsMiddleware rls

provisionVmsGlobalPackages :: [RemoteLogin] -> Action ()
provisionVmsGlobalPackages rls = do
    phPar rls $ \rl -> overSsh rl "sudo add-apt-repository ppa:openjdk-r/ppa -y"
    phPar rls $ \rl -> overSsh rl "sudo apt-get update -y"
    phPar rls $ \rl -> overSsh rl "sudo apt-get install -y build-essential htop libevent-dev openjdk-8-jdk"
    phPar rls $ \rl -> overSsh rl "sudo update-java-alternatives -s /usr/lib/jvm/java-1.8.0-openjdk-amd64"

provisionVmsOrc :: [RemoteLogin] -> Action ()
provisionVmsOrc rls = do
    need [orcBin]
    phPar rls $ \rl -> overSsh rl $ "mkdir -p " ++ takeDirectory orcBin
    phPar rls $ \rl -> rsyncTo rl orcBin orcBin

provisionVmsMemcached :: [RemoteLogin] -> Action ()
provisionVmsMemcached = (`phPar` (`orcRemotely` remoteMemcached))

provisionVmsMemaslap :: [RemoteLogin] -> Action ()
provisionVmsMemaslap = (`phPar` (`orcRemotely` remoteMemaslap))

provisionVmsMiddleware :: [RemoteLogin] -> Action ()
provisionVmsMiddleware rls = phPar rls (\rl -> rsyncTo rl outputJarFile remoteMiddleware)

orcRemotely :: CmdResult r => RemoteLogin -> String -> Action r
orcRemotely rl target = overSsh rl $ unwords [orcBin, "build", target]

eachVm :: (RemoteLogin -> Action ProcessHandle) -> Action ()
eachVm func = do
    vms <- getAllVmsToProvision
    phPar vms func

eachVm' :: (RemoteLogin -> Action ()) -> Action ()
eachVm' func = do
    vms <- getAllVmsToProvision
    forM_ vms func

getAllVmsToProvision :: Action [RemoteLogin]
getAllVmsToProvision = do
    vms <- getRawVmData
    return $ map (\VmData{..} -> RemoteLogin (Just vmAdmin) vmFullUrl) vms

