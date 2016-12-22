module AslBuild.Provision where

import Control.Monad
import Data.List
import System.FilePath
import System.Process

import Development.Shake

import AslBuild.BuildMemcached
import AslBuild.CommonActions
import AslBuild.Constants
import AslBuild.Jar
import AslBuild.Orc
import AslBuild.Types
import AslBuild.Utils
import AslBuild.Vm.Data

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
        clearLocal
    provisionLocalhostGlobalPackagesRule ~> return ()
    provisionLocalhostOrcRule ~> need [orcBin]
    remoteMemcached `byCopying` memcachedBin
    provisionLocalhostMemcachedRule ~> need [remoteMemcached]
    remoteMemaslap `byCopying` memaslapBin
    provisionLocalhostMemaslapRule ~> need [remoteMemaslap]
    remoteMiddleware `byCopying` outputJarFile
    provisionLocalhostMiddlewareRule ~> need [remoteMiddleware]

clearLocal :: Action ()
clearLocal
    -- Kill all servers that may be running
 = do
    (Exit _) <- cmd "killall memcached"
    -- Kill any middleware that is runnning
    (Stdout jps) <- cmd "jps"
    let jpss = lines jps
    forM_ jpss $ \line -> do
        let [pid, name] = words line
        when (name == "asl.jar") $ cmd "kill" pid
    -- Kill all clients that may be running
    (Exit _) <- cmd "killall memaslap"
    (Exit _) <- cmd "killall lt-memaslap"
    return ()

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

clearVmsRule :: String
clearVmsRule = "clear-vms"

provisionVmsRules :: Rules ()
provisionVmsRules = do
    provisionVmsRule ~> (getVmLogins >>= provisionVms)
    provisionVmsGlobalPackagesRule ~> (getVmLogins >>= provisionVmsGlobalPackages)
    provisionVmsOrcRule ~> (getVmLogins >>= provisionVmsOrc)
    provisionVmsMemcachedRule ~> (getVmLogins >>= provisionVmsMemcached)
    provisionVmsMemaslapRule ~> (getVmLogins >>= provisionVmsMemaslap)
    provisionVmsMiddlewareRule ~> (getVmLogins >>= provisionVmsMiddleware)
    clearVmsRule ~> (getVmLogins >>= clearVms)

provisionVms :: [RemoteLogin] -> Action ()
provisionVms =
    postNub $ \rls -> do
        provisionVmsGlobalPackages rls
        provisionVmsOrc rls
        provisionVmsMemcached rls
        provisionVmsMemaslap rls
        provisionVmsMiddleware rls
        clearVms rls

postNub
    :: Eq a
    => ([a] -> b) -> [a] -> b
postNub func ls = func $ nub ls

clearVms :: [RemoteLogin] -> Action ()
clearVms = postNub $ \rls -> parScriptAt $ map (\rl -> (rl, s)) rls
  where
    s = script ["killall memaslap", "killall java", "killall memcached", "true"]

provisionVmsGlobalPackages :: [RemoteLogin] -> Action ()
provisionVmsGlobalPackages =
    postNub $ \rls -> do
        phPar rls $ \rl -> overSsh rl "sudo add-apt-repository ppa:openjdk-r/ppa -y"
        phPar rls $ \rl -> overSsh rl "sudo apt-get update -y"
        phPar rls $ \rl ->
            overSsh rl "sudo apt-get install -y build-essential htop libevent-dev openjdk-8-jdk"
        phPar rls $ \rl ->
            overSsh rl "sudo update-java-alternatives -s /usr/lib/jvm/java-1.8.0-openjdk-amd64"

provisionVmsOrc :: [RemoteLogin] -> Action ()
provisionVmsOrc ls = do
    need [orcBin]
    flip postNub ls $ \rls -> do
        phPar rls $ \rl -> overSsh rl $ "mkdir -p " ++ takeDirectory orcBin
        phPar rls $ \rl -> rsyncTo rl orcBin orcBin

provisionVmsMemcached :: [RemoteLogin] -> Action ()
provisionVmsMemcached = postNub (`phPar` (`orcRemotely` remoteMemcached))

provisionVmsMemaslap :: [RemoteLogin] -> Action ()
provisionVmsMemaslap = postNub (`phPar` (`orcRemotely` remoteMemaslap))

provisionVmsMiddleware :: [RemoteLogin] -> Action ()
provisionVmsMiddleware = postNub (`phPar` (\rl -> rsyncTo rl outputJarFile remoteMiddleware))

orcRemotely
    :: CmdResult r
    => RemoteLogin -> String -> Action r
orcRemotely rl target = overSsh rl $ unwords [orcBin, target]

eachVm :: (RemoteLogin -> Action ProcessHandle) -> Action ()
eachVm func = do
    vms <- getVmLogins
    phPar vms func

eachVm' :: (RemoteLogin -> Action ()) -> Action ()
eachVm' func = do
    vms <- getVmLogins
    forM_ vms func
