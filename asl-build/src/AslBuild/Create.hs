module AslBuild.Create where

import           AslBuild.OptParse

create :: CreateContext -> IO ()
create cctx = do
    putStrLn "Creating servers in the cloud according to this context:"
    print cctx
