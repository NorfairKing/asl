{-# LANGUAGE RecordWildCards #-}

module AslBuild.CommonActions where

import Control.Concurrent
import Control.Monad
import System.Directory hiding (doesFileExist)
import qualified System.Directory as D
import System.Exit
import System.Process

import Development.Shake
import Development.Shake.FilePath

import AslBuild.Memaslap
import AslBuild.Ssh
import AslBuild.Types

onlyIfFileExists :: FilePath -> Rules a -> Rules (Maybe a)
onlyIfFileExists file rule = do
    exists <- liftIO $ D.doesFileExist file
    if exists
        then Just <$> rule
        else return Nothing

-- Depend on a file, but don't rebuild it evern once it exists.
needsToExist :: FilePath -> Action ()
needsToExist file = do
    exists <- doesFileExist file
    need [file | not exists]

phPar :: [a] -> (a -> Action ProcessHandle) -> Action ()
phPar ls func = do
    phs <- forM ls func
    ecs <- liftIO $ mapM waitForProcess phs
    unless (all (== ExitSuccess) ecs) $ fail "Parallel action failed."

wait :: Int -> Action ()
wait i = do
    putLoud $ unwords ["Waiting for", show i, "seconds."]
    liftIO $ threadDelay $ i * 1000 * 1000

waitMs :: Int -> Action ()
waitMs i = do
    putLoud $ unwords ["Waiting for", show i, "milliseconds."]
    liftIO $ threadDelay $ i * 1000

scriptAt
    :: CmdResult r
    => RemoteLogin -> Script -> Action r
scriptAt rl s = do
    unit $ copyScriptOver rl s
    runRemoteVerboseScript rl s

scriptPath :: Script -> FilePath
scriptPath Script {..} = "/tmp" </> scriptName <.> "bash"

copyScriptOver
    :: CmdResult r
    => RemoteLogin -> Script -> Action r
copyScriptOver rl s@Script {..} = do
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
    phPar ss $ uncurry copyScriptOver
    phPar ss $ uncurry runRemoteVerboseScript

parScriptAtResult
    :: CmdResult r
    => [(RemoteLogin, Script)] -> Action [r]
parScriptAtResult ss = do
    phPar ss $ uncurry copyScriptOver
    forP ss $ uncurry runRemoteVerboseScript

runRemoteVerboseScript
    :: CmdResult b
    => RemoteLogin -> Script -> Action b
runRemoteVerboseScript rl s = do
    let fullScript = unlines $ scriptContent s
    liftIO $ putStrLn $ "Running on " ++ remoteLoginStr rl ++ ":\n" ++ fullScript
    overSsh rl $ scriptPath s

overSsh
    :: CmdResult r
    => RemoteLogin -> String -> Action r
overSsh rl commandOverSsh = do
    need [customSshKeyFile, customSshConfigFile]
    cmd "ssh" "-i" customSshKeyFile "-F" customSshConfigFile (remoteLoginStr rl) commandOverSsh

copySshIdTo :: RemoteLogin -> Action ()
copySshIdTo rl = do
    need [customSshKeyFile]
    pubkey <- readFile' customSshPublicKeyFile
    overSsh rl $ "echo " ++ pubkey ++ " >> ~/.ssh/authorized_keys"

rsyncTo
    :: CmdResult r
    => RemoteLogin -> FilePath -> FilePath -> Action r
rsyncTo rl localThing remoteThing = do
    need [customSshKeyFile, customSshConfigFile]
    need [localThing]
    cmd
        "rsync"
        "--compress"
        "--progress"
        "-e"
        [unwords ["ssh", "-i", customSshKeyFile, "-F", customSshConfigFile]]
        localThing
        (remoteLoginStr rl ++ ":" ++ remoteThing)

rsyncFrom
    :: CmdResult r
    => RemoteLogin -> FilePath -> FilePath -> Action r
rsyncFrom rl remoteThing localThing = do
    need [customSshKeyFile, customSshConfigFile]
    -- Ensure that the local directory exists
    unit $ cmd "mkdir" "--parents" $ takeDirectory localThing
    cmd
        "rsync"
        "--compress"
        "--progress"
        "-e"
        [unwords ["ssh", "-i", customSshKeyFile, "-F", customSshConfigFile]]
        (remoteLoginStr rl ++ ":" ++ remoteThing)
        localThing

writeMemaslapConfig :: FilePath -> MemaslapConfig -> Action ()
writeMemaslapConfig file config = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory file
    writeFile' file $ renderMemaslapConfig config
