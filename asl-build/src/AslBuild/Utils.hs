module AslBuild.Utils where

import           Control.Monad

import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath

byCopying :: FilePath -> FilePath -> Rules ()
byCopying to from = to %> \_ -> copyFileChanged from to

absFilesInDir :: FilePath -> [FilePath] -> Action [FilePath]
absFilesInDir dir pats = map (dir </>) <$> getDirectoryFiles dir pats

forP_ :: [a] -> (a -> Action ()) -> Action ()
forP_ ls a = void $ forP ls a

indexed :: [a] -> [(Int, a)]
indexed = zip [1..]

getStrictConfig :: String -> Action String
getStrictConfig key = do
    mval <- getConfig key
    case mval of
        Nothing -> fail $ "Could not find key: " ++ key ++ " in config file."
        Just val -> return val


toClockString :: Int -> String
toClockString i
    | i >= 60 * 60 = show hours   ++ "h" ++ show minutes ++ "m" ++ show seconds ++ "s"
    | i >= 60      = show minutes ++ "m" ++ show seconds ++ "s"
    | otherwise    = show seconds ++ "s"
  where
    hours   = i `quot` (60 * 60)
    minutes = (i `rem` (60 * 60)) `quot` 60
    seconds = i `rem` 60

