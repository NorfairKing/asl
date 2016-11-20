{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module AslBuild.Experiment.Types where

import           Data.Aeson
import           Data.List
import           Data.List.Split
import           GHC.Generics

import           Development.Shake
import           Development.Shake.FilePath

import           Data.Csv                   hiding (lookup, (.:), (.=))
import qualified Data.Csv                   as CSV (lookup)

import           AslBuild.Client.Types
import           AslBuild.Constants
import           AslBuild.Middle.Types
import           AslBuild.Server.Types
import           AslBuild.Types

class ExperimentConfig a where
    highLevelConfig :: a -> HighLevelConfig
    genExperimentSetups :: a -> Action ([ExperimentSetup], [RemoteLogin])

experimentTarget :: ExperimentConfig a => a -> String
experimentTarget = target . highLevelConfig

data HighLevelConfig
    = HighLevelConfig
    { target             :: String
    , nrClients          :: Int
    , nrServers          :: Int
    , location           :: Location
    , resultsPersistence :: Persistence
    } deriving (Show, Eq, Generic)

instance ToJSON   HighLevelConfig
instance FromJSON HighLevelConfig

data ExperimentSetup
    = ExperimentSetup
    { esRuntime            :: TimeUnit
    , esResultsSummaryFile :: FilePath
    , esSetupFile          :: FilePath
    , clientSetups         :: [ClientSetup]
    , backendSetup
        :: Either ServerSetup (MiddleSetup, [ServerSetup])
        -- ^ Left if it's a baseline experiment, right otherwise
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentSetup
instance FromJSON ExperimentSetup

data ExperimentSuccess
    = ExperimentSuccess
    | ExperimentFailure String
    deriving (Show, Eq)

data ExperimentResultSummary
    = ExperimentResultSummary
    { erClientLogFiles     :: [FilePath]
    , merMiddleResultsFile :: Maybe FilePath
    , erSetupFile          :: FilePath --TODO add logfile here.
    } deriving (Show, Eq, Generic)

instance ToJSON   ExperimentResultSummary where
    toJSON ExperimentResultSummary{..} = object
        [ "client-log-files" .= erClientLogFiles
        , "middle-results-file" .= merMiddleResultsFile
        , "setup" .= erSetupFile
        ]

instance FromJSON ExperimentResultSummary where
    parseJSON (Object o) = do
        mrf <- o .: "middle-results-file"
        setup <- o .: "setup"
        moldclrfs <- o .:? "client-results-files"
        -- TODO remove this once we've gotten rid of deprecated files
        clrfs <- case moldclrfs of
            Nothing -> o .: "client-log-files"
            Just oldclrfs ->
                case mapM (stripPrefix $ tmpDir ++ "/") oldclrfs of
                    Nothing -> mempty
                    Just clrfs -> return $ flip map clrfs $ \clrf ->
                            let [eDir, "client-results", file] = splitDirectories clrf
                                [prefix, suffix] = splitOn "client-results" file
                                file' = prefix ++ "client-local-log" ++ suffix
                            in joinPath [resultsDir, eDir, "local-client-logs", file']
        return ExperimentResultSummary
            { merMiddleResultsFile = mrf
            , erSetupFile = setup
            , erClientLogFiles = clrfs
            }
    parseJSON _ = mempty

data MiddleResultLine
    = MiddleResultLine
    { requestKind          :: RequestKind
    , requestReceivedTime  :: Integer
    , requestParsedTime    :: Integer
    , requestEnqueuedTime  :: Integer
    , requestDequeuedTime  :: Integer
    , requestAskedTime     :: Integer
    , requestRepliedTime   :: Integer
    , requestRespondedTime :: Integer
    } deriving (Show, Eq, Generic)

instance ToJSON   MiddleResultLine
instance FromJSON MiddleResultLine

instance FromNamedRecord MiddleResultLine where
    parseNamedRecord r = MiddleResultLine
        <$> CSV.lookup r "Kind"
        <*> CSV.lookup r "ReceivedTime"
        <*> CSV.lookup r "ParsedTime"
        <*> CSV.lookup r "EnqueuedTime"
        <*> CSV.lookup r "DequeuedTime"
        <*> CSV.lookup r "AskedTime"
        <*> CSV.lookup r "RepliedTime"
        <*> CSV.lookup r "RespondedTime"
