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

data EntireCluster
    = EntireCluster
    { ecResourceGroup  :: ResourceGroup
    , ecNrServers      :: Int
    , ecNrClients      :: Int
    , ecClientTemplate :: MachineTemplate
    , ecMiddleTemplate :: MachineTemplate
    , ecServerTemplate :: MachineTemplate
    } deriving (Show, Eq)

data MachineTemplate
    = MachineTemplate
    { stSize          :: String
    , stNamePrefix    :: String
    , stOsType        :: String
    , stAdminUsername :: String
    , stAdminPassword :: String
    , stImageUrn      :: String
    } deriving (Show, Eq)
