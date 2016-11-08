module AslBuild.Vm.Stop where

import           Development.Shake

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Vm.Data
import           AslBuild.Vm.Names
import           AslBuild.Vm.Types

stopVmsRule :: String
stopVmsRule = "stop-vms"

stopVmRules :: Rules ()
stopVmRules =
    stopVmsRule ~> (getVmNames >>= stopVmsByName)

stopVms :: [VmData] -> Action ()
stopVms = stopVmsByName . map vmName

stopVmsByName :: [String] -> Action ()
stopVmsByName ls = do
    phPar ls $ \n ->
        cmd azureCmd "vm" "deallocate"
            "--resource-group" resourceGroupName
            "--name" n

    clearVmData
