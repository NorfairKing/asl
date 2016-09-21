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
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import           Development.Shake       (Rules)
import           Options.Applicative
import           System.Exit

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
    | CreateVms
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
        [ command "resource-group" parseCreateResourceGroup
        , command "vms" parseCreateVms
        ]
    modifier = fullDesc
            <> progDesc "Create the appropriate servers on azure"

parseCreateResourceGroup :: ParserInfo CreateCommand
parseCreateResourceGroup = info parser modifier
  where
    parser = pure CreateResourceGroup
    modifier = fullDesc
            <> progDesc "Create the resource group."

parseCreateVms :: ParserInfo CreateCommand
parseCreateVms = info parser modifier
  where
    parser = pure CreateVms
    modifier = fullDesc
            <> progDesc "Create the virtual machines."

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
    = CreateContextResourceGroup CreateResourceGroupContext
    | CreateContextVms CreateVmsContext
    deriving (Show, Eq)

data CreateResourceGroupContext
    = CreateResourceGroupContext
    { crgcName     :: String
    , crgcLocation :: String
    } deriving (Show, Eq)

data CreateVmsContext
    = CreateVmsContext
    { cvmscResourceGroup :: String
    , cvmscLocation      :: String
    , cvmscNrServers     :: Int
    , cvmscNrClients     :: Int
    , cvmsClientTemplate :: ServerTemplate
    , cvmsMiddleTemplate :: ServerTemplate
    , cvmsServerTemplate :: ServerTemplate
    } deriving (Show, Eq)

data ServerTemplate
    = ServerTemplate
    { stSize          :: String
    , stNamePrefix    :: String
    , stOsType        :: String
    , stAdminUsername :: String
    , stAdminPassword :: String
    , stImageUrn      :: String
    } deriving (Show, Eq)

data CreateVmContext
    = CreateVmContext
    { cvmcSize              :: String
    , cvmcResourceGroupName :: String
    , cvmcName              :: String
    , cvmcLocation          :: String
    , cvmcOs                :: String
    , cvmcAdminUsername     :: String
    , cvmcAdminPassword     :: String
    , cvmcImageUrn          :: String
    } deriving (Show, Eq)

data Settings = Settings
    deriving (Show, Eq)

data Configuration
    = Configuration
    { confPrivate :: PrivateConfiguration
    , confCreate  :: CreateConfiguration
    } deriving (Show, Eq)

data PrivateConfiguration
    = PrivateConfiguration
    { pconfSubscriptionId :: Maybe String
    , pconfResourceGroup  :: ResourceGroupConfig
    } deriving (Show, Eq)

data ResourceGroupConfig
    = ResourceGroupConfig
    { rgcName     :: Maybe String
    , rgcLocation :: Maybe String
    } deriving (Show, Eq)

data CreateConfiguration
    = CreateConfiguration
    { cconfVmsConfig :: CreateVmsConfiguration
    } deriving (Show, Eq)

data CreateVmsConfiguration
    = CreateVmsConfiguration
    { cconfVmsNrServers           :: Maybe Int
    , cconfVmsNrClients           :: Maybe Int
    , cconfVmsClientConfiguration :: MachineConfiguration
    , cconfVmsMiddleConfiguration :: MachineConfiguration
    , cconfVmsServerConfiguration :: MachineConfiguration
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
configToPrivateConfiguration c = PrivateConfiguration
    <$> lookup c "subscription-id"
    <*> parseResourceGroup c

parseResourceGroup :: Config -> IO ResourceGroupConfig
parseResourceGroup c = ResourceGroupConfig
    <$> lookup c "resource-group.name"
    <*> lookup c "resource-group.location"

createConfigFile :: Flags -> FilePath
createConfigFile Flags{..} = fromMaybe defaultCreateConfigFile flagCreateConfig

defaultCreateConfigFile :: FilePath
defaultCreateConfigFile = "create.cfg"

configToCreateConfiguration :: Config -> IO CreateConfiguration
configToCreateConfiguration c = CreateConfiguration
    <$> configToCreateVmsConfiguration c

configToCreateVmsConfiguration :: Config -> IO CreateVmsConfiguration
configToCreateVmsConfiguration c = CreateVmsConfiguration
    <$> lookup c "vms.nr-clients"
    <*> lookup c "vms.nr-servers"
    <*> machineConfiguration c "vms.client-machine"
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

creationConfig :: CreateCommand -> Configuration -> IO CreateContext
creationConfig cc Configuration{..} = case cc of
    CreateResourceGroup ->
        CreateContextResourceGroup
            <$> (CreateResourceGroupContext
                <$> fromMaybe
                    (die "No name configured")
                    (pure <$> rgcName (pconfResourceGroup confPrivate))
                <*> fromMaybe
                    (die "No location configured")
                    (pure <$> rgcLocation (pconfResourceGroup confPrivate)))
    CreateVms -> do
        let vmsc = cconfVmsConfig confCreate
            prg = pconfResourceGroup confPrivate
        CreateContextVms
            <$> (CreateVmsContext
                <$> fromMaybe
                    (die "resource-group.name not configured")
                    (pure <$> rgcName prg)
                <*> fromMaybe
                    (die "resource-group.location not configured")
                    (pure <$> rgcLocation prg)
                <*> fromMaybe
                    (die "vms.nr-clients not configured")
                    (pure <$> cconfVmsNrClients vmsc)
                <*> fromMaybe
                    (die "vms.nr-servers not configured")
                    (pure <$> cconfVmsNrServers vmsc)
                <*> fromMaybe
                    (die "vms.client-machine not configured")
                    (pure <$> machineConfigToServerTemplate (cconfVmsClientConfiguration vmsc))
                <*> fromMaybe
                    (die "vms.middle-machine not configured")
                    (pure <$> machineConfigToServerTemplate (cconfVmsMiddleConfiguration vmsc))
                <*> fromMaybe
                    (die "vms.server-machine not configured")
                    (pure <$> machineConfigToServerTemplate (cconfVmsServerConfiguration vmsc)))


machineConfigToServerTemplate :: MachineConfiguration -> Maybe ServerTemplate
machineConfigToServerTemplate MachineConfiguration{..} = ServerTemplate
    <$> mcSize
    <*> mcNamePrefix
    <*> mcOsType
    <*> mcAdminUsername
    <*> mcAdminPassword
    <*> mcImageUrn




