module AslBuild.Vm.Stop where

import Development.Shake

import AslBuild.CommonActions
import AslBuild.Constants
import AslBuild.Experiment
import AslBuild.Experiments.MaximumThroughput
import AslBuild.Experiments.ReplicationEffect
import AslBuild.Experiments.StabilityTrace
import AslBuild.Experiments.WriteEffect
import AslBuild.Vm.Data
import AslBuild.Vm.Names
import AslBuild.Vm.Types

stopVmsRule :: String
stopVmsRule = "stop-vms"

stopVmRules :: Rules ()
stopVmRules = do
    stopVmsRule ~> stopVmsByName allVmNames
    experimentStopVmsRules

stopVms :: [VmData] -> Action ()
stopVms = stopVmsByName . map vmName

stopVmsByName :: [String] -> Action ()
stopVmsByName ls = do
    phPar ls $ \n -> cmd azureCmd "vm" "deallocate" "--resource-group" resourceGroupName "--name" n
    clearVmData

experimentStopVmsRules :: Rules ()
experimentStopVmsRules = do
    mapM_ makeStopVmRule allMaximumThroughputExperiments
    mapM_ makeStopVmRule allStabilityTraceExperiments
    mapM_ makeStopVmRule allWriteEffectExperiments
    mapM_ makeStopVmRule allReplicationEffectExperiments

stopVmRuleFor
    :: ExperimentConfig a
    => a -> String
stopVmRuleFor ecf = "stop-" ++ experimentTarget ecf ++ "-vms"

makeStopVmRule
    :: ExperimentConfig a
    => a -> Rules ()
makeStopVmRule ecf = stopVmRuleFor ecf ~> stopVmsByName (vmNamesForHLConfig $ highLevelConfig ecf)
