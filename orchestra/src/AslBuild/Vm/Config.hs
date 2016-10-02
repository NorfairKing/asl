module AslBuild.Vm.Config where

import           Development.Shake

import           AslBuild.Utils

getResourceGroupName :: Action String
getResourceGroupName = getStrictConfig "resource-group-name"

getResourceGroupLocation :: Action String
getResourceGroupLocation = getStrictConfig "resource-group-location"
