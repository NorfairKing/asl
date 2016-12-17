module AslBuild.Models where

import           Development.Shake

import           AslBuild.Models.MM1
import           AslBuild.Models.MMm
import           AslBuild.Models.MyModel

modelsRules :: Rules ()
modelsRules = do
    mm1Rules
    mmmRules
    myModelRules
