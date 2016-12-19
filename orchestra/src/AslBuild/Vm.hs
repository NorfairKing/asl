module AslBuild.Vm where

import Development.Shake

import AslBuild.Vm.Data
import AslBuild.Vm.Start
import AslBuild.Vm.Stop

vmRules :: Rules ()
vmRules = do
    vmDataRules
    startVmRules
    stopVmRules
