module AslBuild.OptParse
    ( module AslBuild.OptParse
    , module Control.Monad.Reader
    ) where

import           Control.Monad.Reader
import           Data.Monoid
import           Development.Shake    (Rules)
import           Options.Applicative

getSetting :: (Flags -> a) -> AslBuilder a
getSetting func = asks $ func . snd

getCommand :: AslBuilder Command
getCommand = asks fst

getArguments :: [String] -> IO Arguments
getArguments args = do
    let result = runArgumentsParser args
    handleParseResult result

type AslBuilder = ReaderT Arguments Rules
type Arguments = (Command, Flags)
data Command
    = CommandClean
    | CommandBuild
    | CommandRun
    deriving (Show, Eq)

data Flags = Flags
    { flagsTravis :: Bool
    } deriving (Show, Eq)

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure pfs argParser
  where
    pfs = ParserPrefs
      { prefMultiSuffix = "NOTE"
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
    description = "Note"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat
    [ command "build" parseBuild
    , command "clean" parseClean
    , command "run"   parseRun
    ]

parseClean :: ParserInfo Command
parseClean = info parser modifier
  where
    parser = pure CommandClean
    modifier = fullDesc
            <> progDesc "Clean up generated files."

parseBuild :: ParserInfo Command
parseBuild = info parser modifier
  where
    parser = pure CommandBuild
    modifier = fullDesc
            <> progDesc "Build everything"

parseRun :: ParserInfo Command
parseRun = info parser modifier
  where
    parser = pure CommandRun
    modifier = fullDesc
            <> progDesc "Run the system"

parseFlags :: Parser Flags
parseFlags = Flags
    <$> switch
        ( long "travis"
        <> help "Run on travis")

