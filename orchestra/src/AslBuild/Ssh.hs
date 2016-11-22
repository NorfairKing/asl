module AslBuild.Ssh where

import           System.Directory           (getHomeDirectory)

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants

sshRule :: String
sshRule = "ssh"

customSshKeyFile :: FilePath
customSshKeyFile = tmpDir </> "custom_ssh_key"

customSshPublicKeyFile :: FilePath
customSshPublicKeyFile = customSshKeyFile <.> "pub"

customSshConfigFile :: FilePath
customSshConfigFile = tmpDir </> "ssh_config.cfg"

customKnownHostsFile :: FilePath
customKnownHostsFile = tmpDir </> "ssh_known_hosts"

sshRules :: Rules ()
sshRules = do
    sshRule ~> need
        [ customSshKeyFile
        , customSshPublicKeyFile
        , customSshConfigFile
        , customKnownHostsFile
        ]

    [customSshKeyFile, customSshPublicKeyFile] &%> \_ -> do
        unit $ cmd "ssh-keygen"
            "-t" "rsa"
            "-C" "whatever@example.com"
            "-P" [""]
            "-N" [""]
            "-f" customSshKeyFile
        liftIO $ do
            home <- getHomeDirectory
            pubKey <- readFile customSshPublicKeyFile
            appendFile (home </> ".ssh" </> "authorized_keys") pubKey


    customKnownHostsFile %> \_ ->
        writeFile' customKnownHostsFile ""

    customSshConfigFile %> \_ -> do
        need [customKnownHostsFile, customSshPublicKeyFile]
        writeFile' customSshConfigFile $ unlines
            [ "Host *"
            , "  StrictHostKeyChecking no"
            , "  IdentityFile " ++ customSshPublicKeyFile
            , "  UserKnownHostsFile " ++ customKnownHostsFile
            ]
