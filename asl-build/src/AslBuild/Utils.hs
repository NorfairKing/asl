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
