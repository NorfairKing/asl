module AslBuild.Vm.Start where

import           Development.Shake

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Vm.Names
import           AslBuild.Vm.Types

startVmsRule :: String
startVmsRule = "start-vms"

startVmRules :: Rules ()
startVmRules =
    startVmsRule ~> (getVmNames >>= startVmsByName)

startVms :: [VmData] -> Action ()
startVms = startVmsByName . map vmName

startVmsByName :: [String] -> Action ()
startVmsByName ls = phPar ls $ \n ->
    cmd azureCmd "vm" "start"
        "--resource-group" resourceGroupName
        "--name" n

