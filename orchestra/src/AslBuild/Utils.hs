module AslBuild.Utils where

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy       as LB
import           System.Directory           (createDirectoryIfMissing)

import           Development.Shake
import           Development.Shake.FilePath


byCopying :: FilePath -> FilePath -> Rules ()
byCopying to from = to %> \_ -> copyFileChanged from to

absFilesInDir :: FilePath -> [FilePath] -> Action [FilePath]
absFilesInDir dir pats = map (dir </>) <$> getDirectoryFiles dir pats

forP_ :: [a] -> (a -> Action ()) -> Action ()
forP_ ls a = void $ forP ls a

indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

toClockString :: Int -> String
toClockString i
    | i >= 60 * 60 = show hours   ++ "h" ++ padMOrS minutes ++ "m" ++ padMOrS seconds ++ "s"
    | i >= 60      = show minutes ++ "m" ++ padMOrS seconds ++ "s"
    | otherwise    = show seconds ++ "s"
  where
    hours   = i `quot` (60 * 60)
    minutes = (i `rem` (60 * 60)) `quot` 60
    seconds = i `rem` 60

pad :: Char -> Int -> String -> String
pad c l s
    | length s < l = replicate (l - length s) c ++ s
    | otherwise = s

padMOrS :: Int -> String
padMOrS = pad '0' 2 . show


readJSON :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSON file = do
    contents <- liftIO $ LB.readFile file
    case A.eitherDecode contents of
        Left err -> fail $ "Failed to parse json file: " ++ file ++ ":\n" ++ err
        Right res -> return res

writeJSON :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJSON file thing = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory file
    LB.writeFile file $ A.encodePretty thing

maybeFlag :: Show a => Char -> Maybe a -> [String]
maybeFlag _ Nothing = []
maybeFlag c (Just v) = ['-' : c : ' ' : show v]
