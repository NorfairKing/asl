module AslBuild.OptParse
    ( module AslBuild.OptParse
    , module Control.Monad.Reader
    ) where

import           Control.Monad.Reader
import           Data.Monoid
import           Development.Shake    (Rules)
import           Options.Applicative

type AslBuilder = ReaderT BuildContext Rules

getSetting :: (BuildContext -> a) -> AslBuilder a
getSetting = asks

getArguments :: [String] -> IO Arguments
getArguments args = do
    let result = runArgumentsParser args
    handleParseResult result

type Arguments = (Command, Flags)
data Command
    = CommandBuild BuildContext
    | CommandRun RunContext
    deriving (Show, Eq)

data BuildContext
    = BuildAll
        { buildTravis :: Bool
        }
    | BuildClean
    | BuildReports
    deriving (Show, Eq)

data RunContext
    = RunBaseLine BaseLineConfig
    deriving (Show, Eq)

data BaseLineConfig
    = BaseLineConfig
    { baseLineNrClients :: Int
    } deriving (Show, Eq)

data Flags = Flags
    deriving (Show, Eq)

data Configuration = Configuration

data Settings = Settings

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
    , command "run"   parseRun
    ]

parseBuild :: ParserInfo Command
parseBuild = info parser modifier
  where
    parser = CommandBuild <$> subp
    subp = hsubparser $ mconcat
        [ command "all"    parseBuildAll
        , command "clean"  parseBuildClean
        , command "reports" parseBuildReports
        ]
    modifier = fullDesc
            <> progDesc "Build parts of the system"

parseBuildAll :: ParserInfo BuildContext
parseBuildAll = info parser modifier
  where
    parser = BuildAll
        <$> switch
            ( long "travis"
            <> help "Run on travis")
    modifier = fullDesc
            <> progDesc "Build all the parts"

parseBuildClean :: ParserInfo BuildContext
parseBuildClean = info parser modifier
  where
    parser = pure BuildClean
    modifier = fullDesc
            <> progDesc "Clean up the built parts"

parseBuildReports :: ParserInfo BuildContext
parseBuildReports = info parser modifier
  where
    parser = pure BuildReports
    modifier = fullDesc
            <> progDesc "Build the reports"

parseRun :: ParserInfo Command
parseRun = info parser modifier
  where
    parser = CommandRun <$> subp
    subp = hsubparser $ mconcat
        [ command "baseline" parseRunBaseLine
        ]
    modifier = fullDesc
            <> progDesc "Run the system"

parseRunBaseLine :: ParserInfo RunContext
parseRunBaseLine = info parser modifier
  where
    parser = RunBaseLine <$> parseBaseLineConfig
    modifier = fullDesc
            <> progDesc "Run the baseline experiment"

parseBaseLineConfig :: Parser BaseLineConfig
parseBaseLineConfig = BaseLineConfig
    <$> option auto
        ( long "nr-clients"
        <> metavar "INT"
        <> help "The number of clients to connect to the server.")

parseFlags :: Parser Flags
parseFlags = pure Flags

