{-# LANGUAGE RecordWildCards #-}
module AslBuild.Vm
    ( module AslBuild.Vm
    , module AslBuild.Vm.Types
    ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.List

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Vm.Types

azureVmJsonFile :: FilePath
azureVmJsonFile = tmpDir </> "azure-vm-data.json"

vmDataFile :: FilePath
vmDataFile = tmpDir </> "raw-vm-data.json"

startVmsRule :: String
startVmsRule = "start-vms"

stopVmsRule :: String
stopVmsRule = "stop-vms"

vmRules :: Rules ()
vmRules = do
    azureVmJsonFile %> \_ ->
        cmd (FileStdout azureVmJsonFile)
            azureCmd "vm" "list-ip-address"
            "--resource-group" resourceGroupName
            "--json"

    vmDataFile %> \_ -> do
        need [azureVmJsonFile]
        contents <- liftIO $ LB.readFile azureVmJsonFile
        case eitherDecode contents :: Either String [VmDataInAzureFormat] of
            Left err -> fail $ "Aeson vm data file failed to decode: " ++ err
            Right vms -> liftIO $ LB.writeFile vmDataFile $ encodePretty $ map azureUnpack vms

    startVmsRule ~> (getVmNames >>= startVmsByName)
    stopVmsRule ~> (getVmNames >>= stopVmsByName)

-- FIXME Quick and hacky
getVmNames :: Action [String]
getVmNames = return $ do
    suffix <- [1..11] :: [Int]
    return $ "foraslvms" ++ show suffix

startVms :: [VmData] -> Action ()
startVms = startVmsByName . map vmName

startVmsByName :: [String] -> Action ()
startVmsByName ls = phPar ls $ \vmName ->
    cmd azureCmd "vm" "start"
        "--resource-group" resourceGroupName
        "--name" vmName

stopVms :: [VmData] -> Action ()
stopVms = stopVmsByName . map vmName

stopVmsByName :: [String] -> Action ()
stopVmsByName ls = do
    phPar ls $ \vmName ->
        cmd azureCmd "vm" "deallocate"
            "--resource-group" resourceGroupName
            "--name" vmName
    removeFilesAfter "" [azureVmJsonFile, vmDataFile]

getRawVmData :: Action [VmData]
getRawVmData = do
    need [vmDataFile]
    contents <- liftIO $ LB.readFile vmDataFile
    case eitherDecode contents :: Either String [VmData] of
        Left err  -> fail $ "Raw vm data file failed to decode: " ++ err
        Right vms -> return vms

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
