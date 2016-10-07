{-# LANGUAGE RecordWildCards #-}
module AslBuild.CommonActions where

import           Control.Concurrent
import           Control.Monad
import           System.Directory
import           System.Process

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Memaslap
import           AslBuild.Ssh
import           AslBuild.Types

phPar :: [a] -> (a -> Action ProcessHandle) -> Action ()
phPar ls func = do
    phs <- forM ls func
    liftIO $ mapM_ waitForProcess phs

wait :: Int -> Action ()
wait i = do
    putLoud $ unwords ["Waiting for", show i, "seconds."]
    liftIO $ threadDelay $ i * 1000 * 1000

waitMs :: Int -> Action ()
waitMs i = do
    putLoud $ unwords ["Waiting for", show i, "milliseconds."]
    liftIO $ threadDelay $ i * 1000

scriptAt :: RemoteLogin -> Script -> Action ()
scriptAt rl s@Script{..} = do
    unit $ copyScriptOver rl s
    let fullScript = unlines scriptContent
    liftIO $ putStrLn $ "Running on " ++ remoteLoginStr rl ++ ":\n" ++ fullScript
    -- Run the script
    overSsh rl $ scriptPath s

scriptPath :: Script -> FilePath
scriptPath Script{..} = "/tmp" </> scriptName <.> "bash"

copyScriptOver :: CmdResult r => RemoteLogin -> Script -> Action r
copyScriptOver rl s@Script{..} = do
    let path = scriptPath s
    let fullScript = unlines scriptContent
    writeFile' path fullScript
    -- Make sure it's executable before we copy the script
    -- rsync will retain the permissions.
    liftIO $ do
        p <- getPermissions path
        setPermissions path (p {executable = True})
    -- Copy over the script
    rsyncTo rl path path

parScriptAt :: [(RemoteLogin, Script)] -> Action ()
parScriptAt ss = do
    mapM_ (unit . uncurry copyScriptOver) ss
    phPar ss $ \(rl, s) -> do
        let fullScript = unlines $ scriptContent s
        liftIO $ putStrLn $ "Running on " ++ remoteLoginStr rl ++ ":\n" ++ fullScript
        overSsh rl $ scriptPath s

overSsh :: CmdResult r => RemoteLogin -> String -> Action r
overSsh rl commandOverSsh = do
    need [customSshKeyFile, customSshConfigFile]
    cmd "ssh"
        "-i" customSshKeyFile
        "-F" customSshConfigFile
        (remoteLoginStr rl)
        commandOverSsh

copySshIdTo :: RemoteLogin -> Action ()
copySshIdTo rl = do
    need [customSshKeyFile]
    pubkey <- readFile' customSshPublicKeyFile
    overSsh rl $ "echo " ++ pubkey ++ " >> ~/.ssh/authorized_keys"

rsyncTo :: CmdResult r => RemoteLogin -> FilePath -> FilePath -> Action r
rsyncTo rl localThing remoteThing = do
    need [customSshKeyFile, customSshConfigFile]
    need [localThing]
    cmd "rsync"
        "--compress"
        "--progress"
        "-e"
        [ unwords
            [ "ssh"
            , "-i", customSshKeyFile
            , "-F", customSshConfigFile
            ]
        ]
        localThing
        (remoteLoginStr rl ++ ":" ++ remoteThing)

rsyncFrom :: CmdResult r => RemoteLogin -> FilePath -> FilePath -> Action r
rsyncFrom rl remoteThing localThing = do
    need [customSshKeyFile, customSshConfigFile]
    cmd "rsync"
        "--compress"
        "--progress"
        "-e"
        [ unwords
            [ "ssh"
            , "-i", customSshKeyFile
            , "-F", customSshConfigFile
            ]
        ]
        (remoteLoginStr rl ++ ":" ++ remoteThing)
        localThing

writeMemaslapConfig :: FilePath -> MemaslapConfig -> Action ()
writeMemaslapConfig file config = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory file
    writeFile' file $ renderMemaslapConfig config
