module AslBuild.Create where

import           Data.List                  (intercalate)

import           Development.Shake
import           Development.Shake.Config
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

create3VmsRule :: String
create3VmsRule = "create-3vms"

create11VmsRule :: String
create11VmsRule = "create-11vms"

deleteVmsRule :: String
deleteVmsRule = "delete-vms"

templateArchive :: FilePath
templateArchive = assignmentDir </> "azure-templates.tar"

template3vmsName :: FilePath
template3vmsName = "template3vms.json"

template3vms :: FilePath
template3vms = tmpDir </> template3vmsName

template11vmsName :: FilePath
template11vmsName = "template11vms.json"

template11vms :: FilePath
template11vms = tmpDir </> template11vmsName

createRules :: Rules ()
createRules = do
    [template3vms, template11vms] &%> \_ ->do
        need [templateArchive]
        withTempDir $ \dir -> do
            unit $ cmd tarCmd
                "--extract"
                "--verbose"
                "--file" templateArchive
                "--directory" dir

            let personalize :: FilePath -> FilePath -> Action ()
                personalize fileIn fileOut = do
                    unit $ cmd (FileStdout fileOut) sedCmd
                        [intercalate ";"
                            [ "s/your_public_SSH_key/id_rsa.pub/g"
                            , "s/your_nethz/tomk/g"
                            , "s/defaultValue\\\":\\ null/defaultValue\\\":\\ \\\"pass\\\"/g"
                            ]]
                        fileIn

            personalize (dir </> template3vmsName) template3vms
            personalize (dir </> template11vmsName) template11vms

    create3VmsRule ~> createResourceGroupFromTemplate template3vms
    create11VmsRule ~> createResourceGroupFromTemplate template11vms

    deleteVmsRule ~> do
        resourceGroupName <- getStrictConfig "resource-group-name"
        cmd azureCmd "group" "delete"
            "--name" resourceGroupName

createResourceGroupFromTemplate :: FilePath -> Action ()
createResourceGroupFromTemplate template = do
    need [template]
    resourceGroupName <- getStrictConfig "resource-group-name"
    resourceGroupLocation <- getStrictConfig "resource-group-location"
    cmd azureCmd "group" "create"
        "--name" resourceGroupName
        "--location" resourceGroupLocation
        "--template-file" template
