module AslBuild.CommitHash where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse

commithash :: String
commithash = "commit"

commithashFile :: FilePath
commithashFile = commithash <.> txtExt

commitHashRules :: AslBuilder ()
commitHashRules = lift $ do
    want [commithashFile]
    commithashFile %> \_ -> do
        alwaysRerun
        -- Make the hash as short as possible with --short
        Stdout hash <- quietly $ cmd "git rev-parse --short=0 --verify HEAD"
        Stdout dirtyStr <- quietly $ cmd "git status --porcelain"
        -- # init to remove newline
        let contents = init hash ++ if null (dirtyStr :: String) then [] else "-dirty"
        writeFileChanged commithashFile contents
