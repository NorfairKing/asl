module AslBuild.Models where

import           Development.Shake

import           AslBuild.Models.MM1

modelsRules :: Rules ()
modelsRules =
    mm1Rules
