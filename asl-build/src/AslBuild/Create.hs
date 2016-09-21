module AslBuild.Create where

import           AslBuild.Create.Creatable
import           AslBuild.OptParse

create :: CreateContext -> IO ()
create cctx = case cctx of
    CreateContextResourceGroup rg -> createOnAzure rg
    CreateContextStorageAccount sa -> createOnAzure sa
    CreateContextVirtualMachine vm -> createOnAzure vm
    CreateContextCluster ec -> createOnAzure ec
