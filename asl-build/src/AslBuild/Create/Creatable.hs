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
    createOnAzure VirtualMachine{..} = azure
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

makeVms :: EntireCluster -> [VirtualMachine]
makeVms EntireCluster{..} =
    let fromTemplate :: MachineTemplate -> Int -> VirtualMachine
        fromTemplate st i = VirtualMachine
            { vmSize              = stSize st
            , vmResourceGroupName = rgName ecResourceGroup
            , vmName              = stNamePrefix st ++ show i
            , vmLocation          = rgLocation ecResourceGroup
            , vmOs                = stOsType st
            , vmAdminUsername     = stAdminUsername st
            , vmAdminPassword     = stAdminPassword st
            , vmImageUrn          = stImageUrn st
            }
        createClient = fromTemplate ecClientTemplate
        createMiddle = fromTemplate ecMiddleTemplate
        createServer = fromTemplate ecServerTemplate
        clients = map createClient [1..ecNrClients]
        middles = map createMiddle [1]
        servers = map createServer [1..ecNrServers]
    in clients ++ middles ++ servers


azure :: [String] -> IO ()
azure = call "azure"

call :: String -> [String] -> IO ()
call cmd args = do
    putStrLn $ unwords $ cmd : args
    callProcess cmd args
