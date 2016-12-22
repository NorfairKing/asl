module AslBuild.CommitHash where

import Development.Shake
import Development.Shake.FilePath

import AslBuild.Constants

commithash :: String
commithash = "commit"

commithashFile :: FilePath
commithashFile = aslDir </> commithash <.> txtExt

commithashRule :: String
commithashRule = "commithash"

cleanCommithashRule :: String
cleanCommithashRule = "clean-commithash"

commitHashRules :: Rules ()
commitHashRules = do
    commithashRule ~> need [commithashFile]
    commithashFile %> \_ -> do
        alwaysRerun
        -- Make the hash as short as possible with --short
        Stdout hash <- quietly $ cmd (Cwd aslDir) "git rev-parse --short=0 --verify HEAD"
        Stdout dirtyStr <- quietly $ cmd (Cwd aslDir) "git status --porcelain"
        -- # init to remove newline
        let contents =
                init hash ++
                if null (dirtyStr :: String)
                    then []
                    else "-dirty"
        writeFileChanged commithashFile contents
    cleanCommithashRule ~> removeFilesAfter "." [commithashFile]
