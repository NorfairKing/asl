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
    | CommandRun Experiment
    | CommandCreate CreateCommand
    | CommandTest
    | CommandClean
    deriving (Show, Eq)

data BuildContext
    = BuildAll
    | BuildClean
    | BuildReports
    | BuildTest
    | BuildRunExperiment Experiment
    | BuildAnalysis
    | BuildTravis
    deriving (Show, Eq)

data Experiment
    = BaselineExperiment
    | LocalExperiment
    deriving (Show, Eq)

data CreateCommand
    = CreateResourceGroup
    | CreateStorageAccount
    | CreateVirtualMachine MachineKind
    | CreateCluster
    deriving (Show, Eq)

data Flags = Flags
    { flagCreateConfig :: Maybe FilePath
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
    [ command "build"       parseBuild
    , command "clean"       parseClean
    , command "test"        parseTest
    , command "run"         parseRun
    , command "create"      parseCreate
    ]

parseBuild :: ParserInfo Command
parseBuild = info parser modifier
  where
    parser = CommandBuild <$> subp
    subp = hsubparser $ mconcat
        [ command "all"      parseBuildAll
        , command "reports"  parseBuildReports
        , command "analysis" parseBuildAnalysis
        , command "travis"   parseBuildTravis
        ]
    modifier = fullDesc
            <> progDesc "Build parts of the system"

parseBuildAll :: ParserInfo BuildContext
parseBuildAll = info parser modifier
  where
    parser = pure BuildAll
    modifier = fullDesc
            <> progDesc "Build all the parts"

parseBuildReports :: ParserInfo BuildContext
parseBuildReports = info parser modifier
  where
    parser = pure BuildReports
    modifier = fullDesc
            <> progDesc "Build the reports"

parseBuildAnalysis :: ParserInfo BuildContext
parseBuildAnalysis = info parser modifier
  where
    parser = pure BuildAnalysis
    modifier = fullDesc
            <> progDesc "Run the analysis scripts"

parseBuildTravis :: ParserInfo BuildContext
parseBuildTravis = info parser modifier
  where
    parser = pure BuildTravis
    modifier = fullDesc
            <> progDesc "Run everything travis will run"

parseClean :: ParserInfo Command
parseClean = info parser modifier
  where
    parser = pure CommandClean
    modifier = fullDesc
            <> progDesc "Clean up"

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
        [ command "baseline"            parseRunBaseLine
        , command "local-experiment"    parseRunLocalExperiment
        ]
    modifier = fullDesc
            <> progDesc "Run the system"

parseRunBaseLine :: ParserInfo Experiment
parseRunBaseLine = info parser modifier
  where
    parser = pure BaselineExperiment
    modifier = fullDesc
            <> progDesc "Run the baseline experiment"

parseRunLocalExperiment :: ParserInfo Experiment
parseRunLocalExperiment = info parser modifier
  where
    parser = pure LocalExperiment
    modifier = fullDesc
            <> progDesc "Run a local experiment to test log processing."

parseCreate :: ParserInfo Command
parseCreate = info parser modifier
  where
    parser = CommandCreate <$> subp
    subp = hsubparser $ mconcat
        [ command "resource-group"  parseCreateResourceGroup
        , command "storage-account" parseCreateStorageAccount
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

parseCreateStorageAccount :: ParserInfo CreateCommand
parseCreateStorageAccount = info parser modifier
  where
    parser = pure CreateStorageAccount
    modifier = fullDesc
            <> progDesc "Create the storage account."

parseCreateVirtualMachine :: ParserInfo CreateCommand
parseCreateVirtualMachine = info parser modifier
  where
    parser = CreateVirtualMachine <$> parseMachineKind
    modifier = fullDesc
            <> progDesc "Create a single virtual machine"

parseMachineKind :: Parser MachineKind
parseMachineKind = argument (eitherReader machineKindReader)
    (help "machine kind (client INT|middle|server INT)")
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
        ( long "creation-config"
        <> value Nothing
        <> metavar "FILE"
        <> help ("The path to the creation config file to use. (Default: " ++ defaultCreateConfigFile ++ ")"))

type Instructions = (Dispatch, Settings)

data Dispatch
    = DispatchBuild BuildContext
    | DispatchClean
    | DispatchTest
    | DispatchRun Experiment
    | DispatchCreate CreateContext
    deriving (Show, Eq)

data CreateContext
    = CreateContextResourceGroup ResourceGroup
    | CreateContextStorageAccount StorageAccount
    | CreateContextVirtualMachine VirtualMachine
    | CreateContextCluster EntireCluster
    deriving (Show, Eq)

data Settings = Settings
    deriving (Show, Eq)

data Configuration
    = Configuration
    { confCreate :: CreateConfiguration
    } deriving (Show, Eq)

data CreateConfiguration
    = CreateConfiguration
    { cconfResourceGroup  :: ResourceGroupConfig
    , cconfStorageAccount :: StorageAccountConfig
    , cconfVmsConfig      :: CreateVmsConfiguration
    } deriving (Show, Eq)

data ResourceGroupConfig
    = ResourceGroupConfig
    { rgcName     :: Maybe String
    , rgcLocation :: Maybe String
    } deriving (Show, Eq)

data StorageAccountConfig
    = StorageAccountConfig
    { sacName :: Maybe String
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
    cconfig <- load [Optional $ createConfigFile flags]
    Configuration
        <$> configToCreateConfiguration cconfig

createConfigFile :: Flags -> FilePath
createConfigFile Flags{..} = fromMaybe defaultCreateConfigFile flagCreateConfig

defaultCreateConfigFile :: FilePath
defaultCreateConfigFile = "create.cfg"

configToCreateConfiguration :: Config -> IO CreateConfiguration
configToCreateConfiguration c = CreateConfiguration
    <$> parseResourceGroup c
    <*> configToStorageAccountConfiguration c
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

configToStorageAccountConfiguration :: Config -> IO StorageAccountConfig
configToStorageAccountConfiguration c = StorageAccountConfig
    <$> lookup c "storage-account.name"

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
        CommandClean      -> pure (DispatchClean, sets)
        CommandTest       -> pure (DispatchTest, sets)
        CommandRun ex     -> pure (DispatchRun ex, sets)
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
        sac = cconfStorageAccount confCreate
        rgc = cconfResourceGroup confCreate
    case cc of
        CreateResourceGroup ->
            CreateContextResourceGroup
                <$> fromMaybe
                    (die "No resource-group configured")
                    (pure <$> configResourceGroup rgc)
        CreateStorageAccount ->
            CreateContextStorageAccount
                <$> (StorageAccount
                    <$> fromMaybe
                        (die "No resource-group configured")
                        (pure <$> configResourceGroup rgc)
                    <*> fromMaybe
                        (die "No storage-account.name configured")
                        (pure <$> sacName sac))
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





