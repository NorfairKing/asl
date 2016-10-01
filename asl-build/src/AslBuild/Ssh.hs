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

sshRules :: Rules ()
sshRules = do
    sshRule ~> need
        [ customSshKeyFile
        , customSshPublicKeyFile
        , customSshConfigFile
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


    customSshConfigFile %> \_ ->
        writeFile' customSshConfigFile $ unlines
            [ "Host *"
            , "  StrictHostKeyChecking no"
            , "  IdentityFile " ++ customSshPublicKeyFile
            ]
