module AslBuild.Utils where

import           Control.Monad

import           Development.Shake
import           Development.Shake.FilePath

byCopying :: FilePath -> FilePath -> Rules ()
byCopying to from = to %> \_ -> copyFileChanged from to

absFilesInDir :: FilePath -> [FilePath] -> Action [FilePath]
absFilesInDir dir pats = map (dir </>) <$> getDirectoryFiles dir pats

forP_ :: [a] -> (a -> Action ()) -> Action ()
forP_ ls a = void $ forP ls a

indexed :: [a] -> [(Int, a)]
indexed = zip [1..]

