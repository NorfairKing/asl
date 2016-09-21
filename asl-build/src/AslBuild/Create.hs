{-# LANGUAGE RecordWildCards #-}
module AslBuild.Create where

import           AslBuild.OptParse

import           System.Process

create :: CreateContext -> IO ()
create cctx = case cctx of
    CreateContextResourceGroup crgc -> createResourceGroup crgc
    CreateContextVms cvmc -> createVms cvmc

createResourceGroup :: CreateResourceGroupContext -> IO ()
createResourceGroup CreateResourceGroupContext{..} =
    callProcess "azure"
        [ "group"
        , "create"
        , "--name", crgcName
        , "--location", crgcLocation
        ]

createVms :: CreateVmsContext -> IO ()
createVms cvms = do
    putStrLn "Creating virtual machines!"
    print cvms
    let cmcs = makeCreateVmContexts cvms
    mapM_ print cmcs

makeCreateVmContexts :: CreateVmsContext -> [CreateVmContext]
makeCreateVmContexts CreateVmsContext{..} =
    let fromTemplate :: ServerTemplate -> Int -> CreateVmContext
        fromTemplate st i = CreateVmContext
            { cvmcSize = stSize st
            , cvmcResourceGroupName = cvmscResourceGroup
            , cvmcName              = stNamePrefix st ++ show i
            , cvmcLocation          = cvmscLocation
            , cvmcOs                = stOsType st
            , cvmcAdminUsername     = stAdminUsername st
            , cvmcAdminPassword     = stAdminPassword st
            , cvmcImageUrn          = stImageUrn st
            }
        createClient = fromTemplate cvmsClientTemplate
        createMiddle = fromTemplate cvmsMiddleTemplate
        createServer = fromTemplate cvmsServerTemplate
        clients = map createClient [1..cvmscNrClients]
        middles = map createMiddle [1]
        servers = map createServer [1..cvmscNrServers]
    in clients ++ middles ++ servers


createVm :: CreateVmContext -> IO ()
createVm CreateVmContext{..} =
    callProcess "azure"
        [ "vm"
        , "create"
        , "--vm-size", cvmcSize
        , "--resource-group", cvmcResourceGroupName
        , "--name", cvmcName
        , "--location", cvmcLocation
        , "--os-type", cvmcOs
        , "--admin-username", cvmcAdminUsername
        , "--admin-password", cvmcAdminPassword
        , "--image-urn", cvmcImageUrn
        ]
