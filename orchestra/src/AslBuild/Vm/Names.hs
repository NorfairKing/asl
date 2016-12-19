{-# LANGUAGE RecordWildCards #-}

module AslBuild.Vm.Names where

import AslBuild.Experiment.Types
import AslBuild.Types

vmNamesForHLConfig :: HighLevelConfig -> [String]
vmNamesForHLConfig HighLevelConfig {..} =
    case location of
        Local -> []
        Remote -> vmNamesForRequirements nrClients 1 nrServers

vmNamesForRequirements
    :: Int -- Nr clients
    -> Int -- Nr middlewares
    -> Int -- Nr servers
    -> [String]
vmNamesForRequirements nrc nrm nrs = clients ++ middlewares ++ servers
  where
    clients = take nrc clientVmNames
    middlewares = take nrm $ filter (`notElem` clients) middlewareVmNames
    servers = take nrs $ filter (\s -> s `notElem` clients && s `notElem` middlewares) serverVmNames

allVmNames :: [String]
allVmNames = a4VmNames ++ a2VmNames

middlewareVmNames :: [String]
middlewareVmNames = a4VmNames

a4VmNames :: [String]
a4VmNames = ["foraslvms11"]

clientVmNames :: [String]
clientVmNames = a2VmNames

serverVmNames :: [String]
serverVmNames = a2VmNames

a2VmNames :: [String]
a2VmNames = do
    suffix <- [1 .. 10] :: [Int]
    return $ "foraslvms" ++ show suffix
