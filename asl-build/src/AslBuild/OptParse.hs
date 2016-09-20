module AslBuild.OptParse where

import           Data.Monoid
import           Options.Applicative
import           System.Environment  (getArgs)

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result


type Arguments = (Command, Flags)
data Command = Command
data Flags = Flags
    { flagsTravis :: Bool
    }

runArgumentsParser :: [[Char]] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs argParser
  where
    prefs = ParserPrefs
      { prefMultiSuffix = "NOTE"
      , prefDisambiguate = True
      , prefShowHelpOnError = True
      , prefShowHelpOnEmpty = True
      , prefBacktrack = True
      , prefColumns = 80
      }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help
  where
    help = fullDesc <> progDesc description
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
