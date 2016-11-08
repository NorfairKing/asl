{-# LANGUAGE RecordWildCards #-}
module AslBuild.Vm.Data
    ( vmDataRules
    , getVms
    , getVmLogins
    , clearVmData
    ) where

import           Control.Monad
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.List                  (intersect)

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Types
import           AslBuild.Utils
import           AslBuild.Vm.Types


azureVmJsonFile :: FilePath
azureVmJsonFile = tmpDir </> "azure-vm-data.json"

vmDataFile :: FilePath
vmDataFile = tmpDir </> "raw-vm-data.json"

clearVmDataRule :: String
clearVmDataRule = "clear-vm-data"

vmDataRules :: Rules ()
vmDataRules = do
    azureVmJsonFile %> \_ ->
        cmd (FileStdout azureVmJsonFile)
            azureCmd "vm" "list-ip-address"
            "--resource-group" resourceGroupName
            "--json"

    vmDataFile %> \_ -> do
        avmData <- getAzureVmData
        writeJSON vmDataFile $ map azureUnpack avmData

    clearVmDataRule ~> clearVmData

getAzureVmData :: Action [VmDataInAzureFormat]
getAzureVmData = do
    need [azureVmJsonFile]
    readJSON azureVmJsonFile

getRawVmData :: Action [VmData]
getRawVmData = do
    need [vmDataFile]
    readJSON vmDataFile

getVmLogins :: Action [RemoteLogin]
getVmLogins = do
    vms <- getRawVmData
    return $ map (\VmData{..} -> RemoteLogin (Just vmAdmin) vmFullUrl) vms

getVms
    :: Int -- Number of clients
    -> Int -- Number of middles
    -> Int -- Number of servers
    -> Action ([VmData], [VmData], [VmData])
getVms nrc nrm nrs = do
    rawVms <- getRawVmData
    let middleElligibles = filter middleElligible rawVms
    let clientOrServerElligibles = filter clientOrServerElligible rawVms
    let clients = take nrc clientOrServerElligibles
    let middles = take nrm middleElligibles
    let servers = take nrs $ drop nrc clientOrServerElligibles
    when (any (not . null) [clients `intersect` middles, middles `intersect` servers, clients `intersect` servers]) $
        fail $ "Vms intersect:\n" ++ LB8.unpack (encodePretty (clients, middles, servers))

    if any (\(a, b) -> a /= length b) [(nrc, clients), (nrm, middles), (nrs, servers)]
    then fail "Something went wrong requesting servers."
    else return (clients, middles, servers)

middleElligible :: VmData -> Bool
middleElligible VmData{..} = vmType == "Basic_A4"

clientOrServerElligible :: VmData -> Bool
clientOrServerElligible VmData{..} = vmType == "Basic_A2"

clearVmData :: Action ()
clearVmData = removeFilesAfter "" [azureVmJsonFile, vmDataFile]
