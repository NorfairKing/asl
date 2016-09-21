module AslBuild.OptParse
    ( module AslBuild.OptParse
    , module Control.Monad.Reader
    ) where

import           Control.Monad.Reader
import           Data.Monoid
import           Development.Shake
import           Options.Applicative

getArguments :: [String] -> IO Arguments
getArguments args = do
    let result = runArgumentsParser args
    handleParseResult result


type AslBuilder = ReaderT Flags Rules
type Arguments = (Command, Flags)
data Command = Command
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
parseCommand = pure Command

parseFlags :: Parser Flags
parseFlags = Flags
    <$> switch
        ( long "travis"
        <> help "Run on travis")

