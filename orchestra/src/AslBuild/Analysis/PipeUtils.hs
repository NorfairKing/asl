module AslBuild.Analysis.PipeUtils where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable

import           Data.Sequence          (Seq, (|>))
import qualified Data.Sequence          as S

import           Pipes                  (Pipe, (>->))
import qualified Pipes                  as P
import qualified Pipes.Prelude          as P

errorLogger :: MonadIO m => Pipe (Either String a) a m v
errorLogger = forever $ do
    eea <- P.await
    case eea of
        Left err -> liftIO $ putStrLn err
        Right res -> P.yield res

windowsSeq :: (Integral i, Monad m) => i -> Pipe a (Seq a) m v
windowsSeq windowSize = do
    initData <- replicateM (fromIntegral windowSize) P.await
    recurse $ S.fromList initData
  where
    recurse dats = do
        P.yield dats
        newdat <- P.await
        let newdats = S.drop 1 dats |> newdat
        recurse newdats

windows :: (Integral i, Monad m) => i -> Pipe a [a] m v
windows windowSize = windowsSeq windowSize >-> P.map toList
