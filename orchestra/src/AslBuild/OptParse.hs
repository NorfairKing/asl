{-# LANGUAGE OverloadedStrings #-}
module AslBuild.OptParse
    ( module AslBuild.OptParse
    , module Control.Monad.Reader
    ) where

import           Prelude              hiding (lookup)

import           Control.Monad.Reader
import           Data.Monoid
import           Options.Applicative

getArguments :: [String] -> IO Arguments
getArguments args = do
    let result = runArgumentsParser args
    handleParseResult result

type Arguments = (Command, Flags)
data Command
    = CommandBuild String -- Target
    | CommandClean
    deriving (Show, Eq)

data Experiment
    = LocalLogTestExperiment
    | LocalBaselineExperiment
    | BigLocalBaselineExperiment
    | RemoteBaselineExperiment
    deriving (Show, Eq)

data Flags = Flags

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure pfs argParser
  where
    pfs = ParserPrefs
      { prefMultiSuffix = "ASL"
      , prefDisambiguate = True
      , prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      , prefBacktrack = True
      , prefColumns = 80
      }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) helpText
  where
    helpText = fullDesc <> progDesc description
    description = "ASL"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> pure Flags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
    [ command "build"       parseBuild
    , command "clean"       parseClean
    ]

parseBuild :: ParserInfo Command
parseBuild = info parser modifier
  where
    parser = CommandBuild <$> strArgument
        (help "the target to build")
    modifier = fullDesc
            <> progDesc "Build parts of the system"

parseClean :: ParserInfo Command
parseClean = info parser modifier
  where
    parser = pure CommandClean
    modifier = fullDesc
            <> progDesc "Clean up"

type Instructions = (Dispatch, Settings)

data Dispatch
    = DispatchBuild String
    | DispatchClean
    deriving (Show, Eq)

data Settings = Settings
    deriving (Show, Eq)

data Configuration = Configuration
    deriving (Show, Eq)

getInstructions :: [String] -> IO (Dispatch, Settings)
getInstructions args = getInstructionsHelper
    (getArguments args)
    (const $ const $ pure Configuration)
    combineToInstructions

getInstructionsHelper
    :: IO (command, flags)
        -- ^ A way to parse 'command' and 'flags' from command-lines options
    -> (command -> flags -> IO configuration)
        -- ^ A way to read 'configuration', may depend on the previously obtained 'command' and 'flags'.
    -> (command -> flags -> configuration -> IO (dispatch, settings))
        -- ^ A way to combine 'command', 'flags' and 'configuration' all together to obtain
        -- both a 'dispatch' and 'settings'.
    -> IO (dispatch, settings)
getInstructionsHelper args getConfig combine = do
    (cmd, flags) <- args
    configuration <- getConfig cmd flags
    combine cmd flags configuration

combineToInstructions :: Command -> Flags -> Configuration -> IO (Dispatch, Settings)
combineToInstructions c _ _ = do
    let sets = Settings
    let d = case c of
            CommandBuild targ -> DispatchBuild targ
            CommandClean      -> DispatchClean
    return (d, sets)

