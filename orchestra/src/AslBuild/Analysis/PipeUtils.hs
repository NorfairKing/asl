{-# LANGUAGE ScopedTypeVariables #-}
module AslBuild.Analysis.PipeUtils where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.IO

import           Development.Shake

import           Data.Csv               (DefaultOrdered (..), FromNamedRecord,
                                         ToNamedRecord)
import qualified Data.Csv               as CSV
import           Data.Dequeue           (BankersDequeue, Dequeue (..))
import qualified Data.Dequeue           as D

import           Pipes                  (Pipe, (>->))
import qualified Pipes                  as P
import qualified Pipes.ByteString       as PB
import qualified Pipes.Csv              as P

errorLogger :: MonadIO m => Pipe (Either String a) a m v
errorLogger = forever $ do
    eea <- P.await
    case eea of
        Left err -> liftIO $ putStrLn err
        Right res -> P.yield res

errorIgnorer :: MonadIO m => Pipe (Either String a) a m v
errorIgnorer = forever $ do
    eea <- P.await
    case eea of
        Left _ -> return ()
        Right res -> P.yield res

class Mean a where
    combines :: [a] -> a
    divide :: Integral i => a -> i -> a
    combine :: a -> a -> a
    combine a1 a2 = combines [a1, a2]
    uncombine :: a -> a -> a

instance Mean Integer where
    combines = sum
    divide i1 i2 = i1 `div` fromIntegral i2
    combine = (+)
    uncombine = (-)

slidingMean :: (Integral i, Monad m, Mean a) => i -> Pipe a a m ()
slidingMean windowSize = do
    initData <- replicateM (fromIntegral windowSize) P.await
    let initSum = combines initData
    recurse ((D.fromList :: [b] -> BankersDequeue b) initData) initSum
  where
    recurse :: (Monad m, Dequeue d, Mean a) => d a -> a -> Pipe a a m ()
    recurse dats s = do
        P.yield $ s `divide` windowSize
        case popFront dats of
            Nothing -> return ()
            Just (oldDat, restQ) -> do
                newdat <- P.await
                let newSum = (s `uncombine` oldDat) `combine` newdat
                let newQ = pushBack restQ newdat
                recurse newQ newSum

transformCsvFile
    :: forall m a b. (MonadIO m, FromNamedRecord a, ToNamedRecord b, DefaultOrdered b)
    => FilePath
    -> FilePath
    -> Pipe a b IO ()
    -> m ()
transformCsvFile infile outfile pipe = liftIO $
    withFile infile ReadMode $ \inHandle ->
        withFile outfile WriteMode $ \outHandle ->
            P.runEffect $
                    P.decodeByName (PB.fromHandle inHandle)
                >-> errorIgnorer
                >-> pipe
                >-> P.encodeByName (CSV.headerOrder (undefined :: b))
                >-> PB.toHandle outHandle

transformCsvFileAction
    :: forall a b. (FromNamedRecord a, ToNamedRecord b, DefaultOrdered b)
    => FilePath
    -> FilePath
    -> Pipe a b IO ()
    -> Action ()
transformCsvFileAction infile outfile pipe = do
    need [infile]
    transformCsvFile infile outfile pipe
