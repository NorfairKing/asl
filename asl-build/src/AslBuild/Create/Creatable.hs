{-# LANGUAGE RecordWildCards #-}
module AslBuild.Create.Creatable where

import           System.Directory
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

instance Creatable StorageAccount where
    createOnAzure StorageAccount{..} =
        azure
            ["storage", "account", "create"
            , "--resource-group", rgName saResourceGroup
            , "--location", rgLocation saResourceGroup
            , "--kind", "Storage"
            , "--sku-name", "GRS"
            , saName
            ]

instance Creatable NetworkInterfaceCard where
    createOnAzure NetworkInterfaceCard{..} =
        azure
            [ "network", "nic", "create"
            , "--resource-group", rgName nicResourceGroup
            , "--location", rgLocation nicResourceGroup
            ]

instance Creatable VirtualMachine where
    createOnAzure VirtualMachine{..} = do
        home <- getHomeDirectory
        azure
            [ "vm", "create"
            , "--resource-group", rgName vmResourceGroup
            , "--name", vmName
            , "--location", rgLocation vmResourceGroup
            , "--os-type", vmOs
            , "--image-urn", vmImageUrn
            , "--admin-username", vmAdminUsername
            , "--admin-password", vmAdminPassword
            , "--ssh-publickey-file", home ++ "/.ssh/id_rsa.pub"
            -- , "--availset-name", TestAvailSet
            -- , "--nic-name", LB-NIC1
            -- , "--vnet-name", TestVnet
            -- , "--vnet-subnet-name", FrontEnd
            -- , "--storage-account-name", computeteststore
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
