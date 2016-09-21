{-# LANGUAGE RecordWildCards #-}
module AslBuild.Create.Types where

data ResourceGroup
    = ResourceGroup
    { rgName     :: String
    , rgLocation :: String
    } deriving (Show, Eq)

data VirtualMachine
    = VirtualMachine
    { vmSize              :: String
    , vmResourceGroupName :: String
    , vmName              :: String
    , vmLocation          :: String
    , vmOs                :: String
    , vmAdminUsername     :: String
    , vmAdminPassword     :: String
    , vmImageUrn          :: String
    } deriving (Show, Eq)

data MachineKind
    = Middle
    | Server Int -- Index, 1-based
    | Client Int
    deriving (Show, Eq)

data MachineSpecs
    = MachineSpecs
    { msClientTemplate :: MachineTemplate
    , msMiddleTemplate :: MachineTemplate
    , msServerTemplate :: MachineTemplate
    } deriving (Show, Eq)

data EntireCluster
    = EntireCluster
    { ecResourceGroup :: ResourceGroup
    , ecNrServers     :: Int
    , ecNrClients     :: Int
    , ecMachineSpecs  :: MachineSpecs
    } deriving (Show, Eq)

data MachineTemplate
    = MachineTemplate
    { mtSize          :: String
    , mtNamePrefix    :: String
    , mtOsType        :: String
    , mtAdminUsername :: String
    , mtAdminPassword :: String
    , mtImageUrn      :: String
    } deriving (Show, Eq)

makeVms :: EntireCluster -> [VirtualMachine]
makeVms EntireCluster{..} = map (vmFromKind ecMachineSpecs ecResourceGroup)
        $  map Client [1 .. ecNrClients]
        ++ [Middle]
        ++ map Server [1 .. ecNrServers]

vmFromKind :: MachineSpecs -> ResourceGroup -> MachineKind -> VirtualMachine
vmFromKind MachineSpecs{..} rg mk =
    case mk of
        Client ix   -> vmFromTemplate rg msClientTemplate ix
        Middle      -> vmFromTemplate rg msMiddleTemplate 1
        Server ix   -> vmFromTemplate rg msServerTemplate ix

vmFromTemplate :: ResourceGroup -> MachineTemplate -> Int -> VirtualMachine
vmFromTemplate ResourceGroup{..} MachineTemplate{..} i = VirtualMachine
    { vmSize              = mtSize
    , vmResourceGroupName = rgName
    , vmName              = mtNamePrefix ++ show i
    , vmLocation          = rgLocation
    , vmOs                = mtOsType
    , vmAdminUsername     = mtAdminUsername
    , vmAdminPassword     = mtAdminPassword
    , vmImageUrn          = mtImageUrn
    }
