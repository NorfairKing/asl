module AslBuild.Vm.Names where

import           Development.Shake

getVmNames :: Action [String]
getVmNames = return $ do
    suffix <- [1..11] :: [Int]
    return $ "foraslvms" ++ show suffix


