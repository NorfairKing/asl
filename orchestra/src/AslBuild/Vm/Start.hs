module AslBuild.Vm.Start where

import           Development.Shake

import           AslBuild.CommonActions
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.MaximumThroughput
import           AslBuild.Experiments.ReplicationEffect
import           AslBuild.Experiments.StabilityTrace
import           AslBuild.Experiments.WriteEffect
import           AslBuild.Vm.Names
import           AslBuild.Vm.Types

startVmsRule :: String
startVmsRule = "start-vms"

startVmRules :: Rules ()
startVmRules = do
    startVmsRule ~> startVmsByName allVmNames
    experimentStartVmsRules

startVms :: [VmData] -> Action ()
startVms = startVmsByName . map vmName

startVmsByName :: [String] -> Action ()
startVmsByName ls = phPar ls $ \n ->
    cmd azureCmd "vm" "start"
        "--resource-group" resourceGroupName
        "--name" n

experimentStartVmsRules :: Rules ()
experimentStartVmsRules = do
    mapM_ makeStartVmRule allMaximumThroughputExperiments
    mapM_ makeStartVmRule allStabilityTraceExperiments
    mapM_ makeStartVmRule allWriteEffectExperiments
    mapM_ makeStartVmRule allReplicationEffectExperiments

startVmRuleFor :: ExperimentConfig a => a -> String
startVmRuleFor ecf = "start-" ++ experimentTarget ecf ++ "-vms"

makeStartVmRule :: ExperimentConfig a => a -> Rules ()
makeStartVmRule ecf = startVmRuleFor ecf ~>
    startVmsByName (vmNamesForHLConfig $ highLevelConfig ecf)
