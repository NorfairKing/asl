{-# LANGUAGE RecordWildCards #-}
module AslBuild.Vm where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy       as LB

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
            "--json"

    vmDataFile %> \_ -> do
        need [azureVmJsonFile]
        contents <- liftIO $ LB.readFile azureVmJsonFile
        case eitherDecode contents :: Either String [VmDataInAzureFormat] of
            Left err -> fail $ "Aeson vm data file failed to decode: " ++ err
            Right vms -> liftIO $ LB.writeFile vmDataFile $ encodePretty $ map azureUnpack vms

    startVmsRule ~> do
        rawVmData <- getRawVmData
        phPar rawVmData $ \VmData{..} ->
            cmd azureCmd "vm" "start"
                "--resource-group" resourceGroupName
                "--name" vmName

    stopVmsRule ~> do
        rawVmData <- getRawVmData
        phPar rawVmData $ \VmData{..} ->
            cmd azureCmd "vm" "stop"
                "--resource-group" resourceGroupName
                "--name" vmName


getRawVmData :: Action [VmData]
getRawVmData = do
    need [vmDataFile]
    contents <- liftIO $ LB.readFile vmDataFile
    case eitherDecode contents :: Either String [VmData] of
        Left err -> fail $ "Raw vm data file failed to decode: " ++ err
        Right vms -> return vms

getVms
    :: Int -- Number of clients
    -> Int -- Number of middles
    -> Int -- Number of servers
    -> Action ([VmData], [VmData], [VmData])
getVms nrc nrm nrs = do
    rawVms <- getRawVmData
    let total = nrc + nrm + nrs
    let nrAvailable = length rawVms
    if total > nrAvailable
    then fail $ unwords
        [ "Requested too many servers:"
        , show total ++ ", only"
        , show nrAvailable
        , "available."
        ]
    else do
        let clients = take nrc rawVms
            middles = take nrm $ drop nrc rawVms
            servers = take nrs $ drop (nrc + nrm) rawVms
        return (clients, middles, servers)

