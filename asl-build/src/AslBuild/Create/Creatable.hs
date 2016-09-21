{-# LANGUAGE RecordWildCards #-}
module AslBuild.Create.Creatable where

import           System.Process

import           AslBuild.Create.Types

class Creatable a where
    createOnAzure :: a -> IO ()

instance Creatable ResourceGroup where
    createOnAzure ResourceGroup{..} = azure
        [ "group"
        , "create"
        , "--name", rgName
        , "--location", rgLocation
        ]

instance Creatable VirtualMachine where
    createOnAzure vm@VirtualMachine{..} = do
        putStrLn $ "creating vm: " ++ show vm
        azure
            [ "vm"
            , "create"
            , "--vm-size", vmSize
            , "--resource-group", vmResourceGroupName
            , "--name", vmName
            , "--location", vmLocation
            , "--os-type", vmOs
            , "--admin-username", vmAdminUsername
            , "--admin-password", vmAdminPassword
            , "--image-urn", vmImageUrn
            ]

instance Creatable EntireCluster where
    createOnAzure ec@EntireCluster{..} = do
        putStrLn "Creating entire cluster"
        print ec
        let vms = makeVms ec
        mapM_ print vms

azure :: [String] -> IO ()
azure = call "azure"

call :: String -> [String] -> IO ()
call cmd args = do
    putStrLn $ unwords $ cmd : args
    callProcess cmd args
