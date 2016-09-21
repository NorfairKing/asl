{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.OptParse
    ( module AslBuild.OptParse
    , module Control.Monad.Reader
    ) where

import           Prelude                 hiding (lookup)

import           Control.Monad.Reader
import           Data.Configurator
import           Data.Configurator.Types (Config)
import           Data.List.Split
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import           Development.Shake       (Rules)
import           Options.Applicative
import           System.Exit

import           AslBuild.Create.Types

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
    | CommandCreate CreateCommand
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

data CreateCommand
    = CreateResourceGroup
    | CreateVirtualMachine MachineKind
    | CreateCluster
    deriving (Show, Eq)

data Flags = Flags
    { flagCreateConfig  :: Maybe FilePath
    , flagPrivateConfig :: Maybe FilePath
    }
    deriving (Show, Eq)

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
    [ command "build"   parseBuild
    , command "run"     parseRun
    , command "create"  parseCreate
    ]

parseBuild :: ParserInfo Command
parseBuild = info parser modifier
  where
    parser = CommandBuild <$> subp
    subp = hsubparser $ mconcat
        [ command "all"     parseBuildAll
        , command "clean"   parseBuildClean
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

parseCreate :: ParserInfo Command
parseCreate = info parser modifier
  where
    parser = CommandCreate <$> subp
    subp = hsubparser $ mconcat
        [ command "resource-group"  parseCreateResourceGroup
        , command "virtual-machine" parseCreateVirtualMachine
        , command "cluster"         parseCreateCluster
        ]
    modifier = fullDesc
            <> progDesc "Create the appropriate servers on azure"

parseCreateResourceGroup :: ParserInfo CreateCommand
parseCreateResourceGroup = info parser modifier
  where
    parser = pure CreateResourceGroup
    modifier = fullDesc
            <> progDesc "Create the resource group."

parseCreateVirtualMachine :: ParserInfo CreateCommand
parseCreateVirtualMachine = info parser modifier
  where
    parser = CreateVirtualMachine <$> parseMachineKind
    modifier = fullDesc
            <> progDesc "Create a single virtual machine"

parseMachineKind :: Parser MachineKind
parseMachineKind = option (eitherReader machineKindReader)
    (long "machine-kind"
    <> help "machine kind (client INT|middle|server INT)")
  where
    machineKindReader :: String -> Either String MachineKind
    machineKindReader s = case splitOn "-" s of
        [] -> Left "No machinekind given."
        ["middle"] -> Right Middle
        [_] -> Left "Not enough arguments to parse a machinekind."
        ["client", ix] -> Right $ Client $ read ix
        ["server", ix] -> Right $ Server $ read ix
        _ -> Left "Too many arguments to make a machinekind."

parseCreateCluster :: ParserInfo CreateCommand
parseCreateCluster = info parser modifier
  where
    parser = pure CreateCluster
    modifier = fullDesc
            <> progDesc "Create the entire cluster."

parseFlags :: Parser Flags
parseFlags = Flags
    <$> option (Just <$> str)
        ( long "private-config"
        <> value Nothing
        <> metavar "FILE"
        <> help ("The path to the private config file to use. (Default: " ++ defaultPrivateConfigFile ++ ")"))
    <*> option (Just <$> str)
        ( long "creation-config"
        <> value Nothing
        <> metavar "FILE"
        <> help ("The path to the creation config file to use. (Default: " ++ defaultCreateConfigFile ++ ")"))

type Instructions = (Dispatch, Settings)

data Dispatch
    = DispatchBuild BuildContext
    | DispatchRun RunContext
    | DispatchCreate CreateContext
    deriving (Show, Eq)

data CreateContext
    = CreateContextResourceGroup ResourceGroup
    | CreateContextVirtualMachine VirtualMachine
    | CreateContextCluster EntireCluster
    deriving (Show, Eq)

data Settings = Settings
    deriving (Show, Eq)

data Configuration
    = Configuration
    { confPrivate :: PrivateConfiguration
    , confCreate  :: CreateConfiguration
    } deriving (Show, Eq)

data PrivateConfiguration
    = PrivateConfiguration
    deriving (Show, Eq)

data ResourceGroupConfig
    = ResourceGroupConfig
    { rgcName     :: Maybe String
    , rgcLocation :: Maybe String
    } deriving (Show, Eq)

data CreateConfiguration
    = CreateConfiguration
    { pconfResourceGroup :: ResourceGroupConfig
    , cconfVmsConfig     :: CreateVmsConfiguration
    } deriving (Show, Eq)

data CreateVmsConfiguration
    = CreateVmsConfiguration
    { cconfVmsNrServers :: Maybe Int
    , cconfVmsNrClients :: Maybe Int
    , cconfVmsMspecs    :: MachineSpecsConfig
    } deriving (Show, Eq)

data MachineSpecsConfig
    = MachineSpecsConfig
    { mscClientConfiguration :: MachineConfiguration
    , mscMiddleConfiguration :: MachineConfiguration
    , mscServerConfiguration :: MachineConfiguration
    } deriving (Show, Eq)

data MachineConfiguration
    = MachineConfiguration
    { mcSize          :: Maybe String
    , mcNamePrefix    :: Maybe String
    , mcOsType        :: Maybe String
    , mcAdminUsername :: Maybe String
    , mcAdminPassword :: Maybe String
    , mcImageUrn      :: Maybe String
    } deriving (Show, Eq)

getInstructions :: [String] -> IO (Dispatch, Settings)
getInstructions args = getInstructionsHelper
    (getArguments args)
    getConfiguration
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

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ flags = do
    pconfig <- load [Optional $ privateConfigFile flags]
    cconfig <- load [Optional $ createConfigFile flags]
    Configuration
        <$> configToPrivateConfiguration pconfig
        <*> configToCreateConfiguration cconfig

privateConfigFile :: Flags -> FilePath
privateConfigFile Flags{..} = fromMaybe defaultPrivateConfigFile flagPrivateConfig

defaultPrivateConfigFile :: FilePath
defaultPrivateConfigFile = "private.cfg"

configToPrivateConfiguration :: Config -> IO PrivateConfiguration
configToPrivateConfiguration _ = pure PrivateConfiguration

createConfigFile :: Flags -> FilePath
createConfigFile Flags{..} = fromMaybe defaultCreateConfigFile flagCreateConfig

defaultCreateConfigFile :: FilePath
defaultCreateConfigFile = "create.cfg"

configToCreateConfiguration :: Config -> IO CreateConfiguration
configToCreateConfiguration c = CreateConfiguration
    <$> parseResourceGroup c
    <*> configToCreateVmsConfiguration c

parseResourceGroup :: Config -> IO ResourceGroupConfig
parseResourceGroup c = ResourceGroupConfig
    <$> lookup c "resource-group.name"
    <*> lookup c "resource-group.location"

configToCreateVmsConfiguration :: Config -> IO CreateVmsConfiguration
configToCreateVmsConfiguration c = CreateVmsConfiguration
    <$> lookup c "vms.nr-clients"
    <*> lookup c "vms.nr-servers"
    <*> configToMachineSpecsConfig c

configToMachineSpecsConfig :: Config -> IO MachineSpecsConfig
configToMachineSpecsConfig c = MachineSpecsConfig
    <$> machineConfiguration c "vms.client-machine"
    <*> machineConfiguration c "vms.middle-machine"
    <*> machineConfiguration c "vms.server-machine"

machineConfiguration :: Config -> Text -> IO MachineConfiguration
machineConfiguration c g = do
    let lo s = lookup c $ g <> "." <> s
    MachineConfiguration
        <$> lo "size"
        <*> lo "name-prefix"
        <*> lo "os-type"
        <*> lo "admin-username"
        <*> lo "admin-password"
        <*> lo "image-urn"

combineToInstructions :: Command -> Flags -> Configuration -> IO (Dispatch, Settings)
combineToInstructions c _ conf = do
    let sets = Settings
    case c of
        CommandBuild bctx -> pure (DispatchBuild bctx, sets)
        CommandRun rctx   -> pure (DispatchRun rctx, sets)
        CommandCreate cc  -> do
                cctx <- creationConfig cc conf
                return (DispatchCreate cctx, sets)

configResourceGroup :: ResourceGroupConfig -> Maybe ResourceGroup
configResourceGroup ResourceGroupConfig{..} = ResourceGroup
    <$> rgcName
    <*> rgcLocation

configMachineSpecs :: MachineSpecsConfig -> Maybe MachineSpecs
configMachineSpecs MachineSpecsConfig{..} = MachineSpecs
    <$> machineConfigToServerTemplate mscClientConfiguration
    <*> machineConfigToServerTemplate mscMiddleConfiguration
    <*> machineConfigToServerTemplate mscServerConfiguration

machineConfigToServerTemplate :: MachineConfiguration -> Maybe MachineTemplate
machineConfigToServerTemplate MachineConfiguration{..} = MachineTemplate
    <$> mcSize
    <*> mcNamePrefix
    <*> mcOsType
    <*> mcAdminUsername
    <*> mcAdminPassword
    <*> mcImageUrn

creationConfig :: CreateCommand -> Configuration -> IO CreateContext
creationConfig cc Configuration{..} = do
    let vmsc = cconfVmsConfig confCreate
        msps = cconfVmsMspecs vmsc
        rgc = pconfResourceGroup confCreate
    case cc of
        CreateResourceGroup ->
            CreateContextResourceGroup
                <$> fromMaybe
                    (die "No resource-group configured")
                    (pure <$> configResourceGroup rgc)
        CreateVirtualMachine mk -> do
            nrc <- fromMaybe
                        (die "vms.nr-clients not configured")
                        (pure <$> cconfVmsNrClients vmsc)
            nrs <- fromMaybe
                        (die "vms.nr-servers not configured")
                        (pure <$> cconfVmsNrServers vmsc)
            ms <- fromMaybe (die "no machine specs configured") $ pure <$> configMachineSpecs msps
            rg <- fromMaybe (die "no resource group configured") $ pure <$> configResourceGroup rgc
            mk' <- case mk of
                Client ix -> if ix < 1 || ix > nrc then die ("Client index out of bounds: " ++ show ix) else return mk
                Middle -> return mk
                Server ix -> if ix < 1 || ix > nrs then die ("Server index out of bounds: " ++ show ix) else return mk

            return $ CreateContextVirtualMachine $ vmFromKind ms rg mk'
        CreateCluster ->
            CreateContextCluster
                <$> (EntireCluster
                    <$> fromMaybe
                        (die "No resource-group configured")
                        (pure <$> configResourceGroup rgc)
                    <*> fromMaybe
                        (die "vms.nr-clients not configured")
                        (pure <$> cconfVmsNrClients vmsc)
                    <*> fromMaybe
                        (die "vms.nr-servers not configured")
                        (pure <$> cconfVmsNrServers vmsc)
                    <*> fromMaybe
                        (die "machinespecs not configured")
                        (pure <$> configMachineSpecs msps))





