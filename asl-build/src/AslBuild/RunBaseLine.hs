{-# LANGUAGE RecordWildCards #-}
module AslBuild.RunBaseLine where

import           AslBuild.OptParse

-- The baseline performance of a single memcached server connected
-- to one or multiple client machines should also be explored.
-- The basic questions to answer are:
-- * What is the maximum throughput of the server without the middleware?
-- * What is the response time without the middleware?
runBaseLine :: BaseLineConfig -> IO ()
runBaseLine conf@BaseLineConfig{..} =
    if baseLineNrClients < 1 || baseLineNrClients > 3
    then putStrLn "Number of clients must be between 1 and 3."
    else do
        putStrLn "Running baseline experiment with this config:"
        print conf
