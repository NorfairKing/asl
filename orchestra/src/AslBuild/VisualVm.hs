module AslBuild.VisualVm where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants

visualVmRule :: String
visualVmRule = "visualvm"

visualVmBin :: FilePath
visualVmBin = outDir </> "visualvm"

visualVmBinInDlDir :: FilePath
visualVmBinInDlDir = tmpDir </> "visualvm_139" </> "bin" </> "visualvm"

visualVmUrl :: FilePath
visualVmUrl = "https://github.com/visualvm/visualvm.src/releases/download/1.3.9/visualvm_139.zip"

visualVmZip :: FilePath
visualVmZip = tmpDir </> "visualvm.zip"

visualVmRules :: Rules ()
visualVmRules = do
    visualVmRule ~> need [visualVmBinInDlDir]

    visualVmZip %> \_ ->
        cmd curlCmd "--output" visualVmZip visualVmUrl "--location" -- Folow redirects

    visualVmBinInDlDir %> \_ -> do
        need [visualVmZip]
        cmd "unzip" "-d" tmpDir visualVmZip
