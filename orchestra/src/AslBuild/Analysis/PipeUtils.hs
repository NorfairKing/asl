{-# LANGUAGE ScopedTypeVariables #-}
module AslBuild.Analysis.PipeUtils where

import           Control.Monad
import           Control.Monad.IO.Class
import           System.IO

import           Development.Shake

import           Data.Csv               (DefaultOrdered (..),
                                         FromNamedRecord (..), ToNamedRecord)
import qualified Data.Csv               as CSV

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

filterMaybes :: Monad m => Pipe (Maybe a) a m r
filterMaybes = forever $ do
    mv <- P.await
    case mv of
        Nothing -> pure ()
        Just v -> P.yield v

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
