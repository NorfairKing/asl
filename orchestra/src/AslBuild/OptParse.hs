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
    | CommandRun Experiment
    | CommandTest
    | CommandClean
    | CommandTravis
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
    , command "test"        parseTest
    , command "run"         parseRun
    , command "travis"      parseTravis
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

parseTravis :: ParserInfo Command
parseTravis = info parser modifier
  where
    parser = pure CommandTravis
    modifier = fullDesc
            <> progDesc "Run continuous integration"

parseTest :: ParserInfo Command
parseTest = info parser modifier
  where
    parser = pure CommandTest
    modifier = fullDesc
            <> progDesc "Run the local tests"

parseRun :: ParserInfo Command
parseRun = info parser modifier
  where
    parser = CommandRun <$> subp
    subp = hsubparser $ mconcat
        [ command "local-experiment"    parseRunLocalLogTestExperiment
        , command "local-baseline"      parseRunLocalBaseline
        , command "big-local-baseline"  parseRunBigLocalBaseline
        , command "remote-baseline"     parseRunRemoteBaseline
        ]
    modifier = fullDesc
            <> progDesc "Run the system"

parseRunLocalLogTestExperiment :: ParserInfo Experiment
parseRunLocalLogTestExperiment = info parser modifier
  where
    parser = pure LocalLogTestExperiment
    modifier = fullDesc
            <> progDesc "Run a local experiment to test log processing."

parseRunLocalBaseline :: ParserInfo Experiment
parseRunLocalBaseline = info parser modifier
  where
    parser = pure LocalBaselineExperiment
    modifier = fullDesc
            <> progDesc "Run the baseline experiment locally"

parseRunBigLocalBaseline :: ParserInfo Experiment
parseRunBigLocalBaseline = info parser modifier
  where
    parser = pure BigLocalBaselineExperiment
    modifier = fullDesc
            <> progDesc "Run the big baseline experiment locally"

parseRunRemoteBaseline :: ParserInfo Experiment
parseRunRemoteBaseline = info parser modifier
  where
    parser = pure RemoteBaselineExperiment
    modifier = fullDesc
            <> progDesc "Run the baseline experiment remotely (on azure)"

type Instructions = (Dispatch, Settings)

data Dispatch
    = DispatchBuild String
    | DispatchClean
    | DispatchTest
    | DispatchRun Experiment
    | DispatchTravis
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
            CommandTest       -> DispatchTest
            CommandRun ex     -> DispatchRun ex
            CommandTravis     -> DispatchTravis
    return (d, sets)

