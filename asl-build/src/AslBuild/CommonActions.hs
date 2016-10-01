{-# LANGUAGE RecordWildCards #-}
module AslBuild.CommonActions where

import           Control.Concurrent
import           System.Directory

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Memaslap
import           AslBuild.Types

wait :: Int -> Action ()
wait i = do
    putLoud $ unwords ["Waiting for", show i, "seconds."]
    liftIO $ threadDelay $ i * 1000 * 1000

scriptAt :: RemoteLogin -> Script -> Action ()
scriptAt rl Script{..} = do
    let path = "/tmp" </> scriptName <.> "bash"
    let fullScript = unlines scriptContent
    writeFile' path fullScript
    -- Make sure it's executable before we copy the script
    -- rsync will retain the permissions.
    liftIO $ do
        p <- getPermissions path
        setPermissions path (p {executable = True})
    -- Copy over the script
    rsyncTo rl path path
    liftIO $ putStrLn $ "Running on " ++ remoteLoginStr rl ++ ":\n" ++ fullScript
    -- Run the script
    overSsh rl path

overSsh :: RemoteLogin -> String -> Action ()
overSsh rl commandOverSsh =
    command [] "ssh" [remoteLoginStr rl, commandOverSsh]

rsyncTo :: RemoteLogin -> FilePath -> FilePath -> Action ()
rsyncTo rl localThing remoteThing = do
    need [localThing]
    command [] "rsync"
        [ localThing
        , remoteLoginStr rl ++ ":" ++ remoteThing
        ]

rsyncFrom :: RemoteLogin -> FilePath -> FilePath -> Action ()
rsyncFrom rl remoteThing localThing =
    command [] "rsync"
        [ remoteLoginStr rl ++ ":" ++ remoteThing
        , localThing
        ]

writeMemaslapConfig :: FilePath -> MemaslapConfig -> Action ()
writeMemaslapConfig file config = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory file
    writeFile' file $ renderMemaslapConfig config
