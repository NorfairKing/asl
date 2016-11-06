module AslBuild.Analysis.PipeUtils where

import           Control.Monad
import           Control.Monad.IO.Class

import           Pipes                  (Pipe)
import qualified Pipes                  as P

errorLogger :: MonadIO m => Pipe (Either String a) a m v
errorLogger = forever $ do
    eea <- P.await
    case eea of
        Left err -> liftIO $ putStrLn err
        Right res -> P.yield res
