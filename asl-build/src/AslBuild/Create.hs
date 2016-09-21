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
createVms cvms@CreateVmsContext{..} = do
    putStrLn "Creating virtual machines!"
    print cvms
