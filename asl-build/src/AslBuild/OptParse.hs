module AslBuild.OptParse where

import           Data.Monoid
import           Options.Applicative

getArguments :: [String] -> IO Arguments
getArguments args = do
    let result = runArgumentsParser args
    handleParseResult result


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
