module AslBuild.Utils where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List

import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Encode.Pretty   as A
import qualified Data.ByteString.Lazy       as LB
import           Data.Csv                   (DefaultOrdered, FromNamedRecord,
                                             ToNamedRecord)
import qualified Data.Csv                   as CSV
import           Data.Hashable              (Hashable)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HM
import           Data.Vector                (Vector)
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

pad0 :: Int -> String -> String
pad0 = pad '0'

padMOrS :: Int -> String
padMOrS = pad0 2 . show

flatPercent :: Double -> String
flatPercent = go
  where
    go d
        | d == 0 = "0"
        | d > 1 = show (floor d :: Integer)
        | otherwise = '0' : go (10 * d)

readJSOND :: (MonadIO m, FromJSON a, FromJSON b) => (a -> b) -> FilePath -> m b
readJSOND func file = do
    contents <- liftIO $ LB.readFile file
    case A.eitherDecode contents of
        Right resb -> pure resb
        Left errb ->
            case A.eitherDecode contents of
                Right resa -> pure $ func resa
                Left erra -> fail $ "Failed to parse json file: " ++ file ++ ":\n" ++ errb ++ "\nand\n" ++ erra

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
maybeFlag _ Nothing  = []
maybeFlag c (Just v) = ['-' : c : ' ' : show v]

writeCSV :: (MonadIO m, DefaultOrdered a, ToNamedRecord a) => FilePath -> [a] -> m ()
writeCSV file entries = liftIO $ LB.writeFile file $ CSV.encodeDefaultOrderedByName entries

readCSV :: (MonadIO m, FromNamedRecord a) => FilePath -> m (Vector a)
readCSV file = do
    contents <- liftIO $ LB.readFile file
    case CSV.decodeByName contents of
        Left err       -> fail $ "Failed to decode CSV file: " ++ err
        Right (_, vec) -> return vec

zipCombineLists :: Monoid a => [[a]] -> [a]
zipCombineLists = map mconcat . transpose

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither dv Nothing = Left dv
maybeToEither _ (Just v) = Right v

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _        = error "fromLeft: does not work for 'Right'"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _         = error "fromRight: does not work for 'Left'"

changeFilename :: (FilePath -> FilePath) -> FilePath -> FilePath
changeFilename func path = dir </> func file ++ exts
  where
    (full, exts) = splitExtensions path
    (dir, file) = splitFileName full

mapKeys :: (Eq l, Hashable l) => (k -> l) -> HashMap k v -> HashMap l v
mapKeys func hm = HM.fromList $ map (first func) $ HM.toList hm


replaceSndDir :: FilePath -> FilePath -> FilePath
replaceSndDir file d =
    case reverse dirs of
        []      -> d </> file
        [f]     -> d </> f
        (f:z:_) -> d </> z </> f
   where dirs = splitDirectories file
