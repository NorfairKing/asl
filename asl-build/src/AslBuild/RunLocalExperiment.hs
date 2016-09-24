module AslBuild.RunLocalExperiment where

import           Control.Exception
import           Data.List
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process

import           AslBuild.Memcached
import           AslBuild.OptParse

runLocalExperiment :: IO ()
runLocalExperiment = do
    logExists <- doesFileExist logFile
    unless logExists $
        bracket
            startMemcached
            terminateProcess
            $ const runMemaslap

    explog <- readFile logFile
    let parsedLog = parseLog explog
    print parsedLog


logFile :: FilePath
logFile = "tmp/memaslaplog.txt"

memaslapConfigFile :: FilePath
memaslapConfigFile = "tmp/distribution.txt"

startMemcached :: IO ProcessHandle
startMemcached = do
    (_, _, _, ph) <- createProcess $ proc memcachedBin []
    return ph

runMemaslap :: IO ()
runMemaslap =
    withFile logFile WriteMode $ \outh -> do
        (_, _, _, ph) <- createProcess $ (proc memaslapBin
            ["-s", "localhost:11211"
            , "--threads=64"
            , "--concurrency=64"
            , "--overwrite=1"
            , "--stat_freq=2s"
            , "--time=2s"
            , "--cfg_cmd=" ++ memaslapConfigFile
            ]) { std_out = UseHandle outh }
        ec <- waitForProcess ph
        case ec of
            ExitSuccess -> return ()
            ExitFailure eci -> do
                putStrLn $ "memaslap failed with exit code " ++ show eci ++ " aborting"
                exitFailure

data ParsedLog
    = ParsedLog
    { avg :: Double
    , std :: Double
    , tps :: Double
    } deriving (Show, Eq)

parseLog :: String -> Maybe ParsedLog
parseLog s = do
        -- Required because there are also set statistics.
    let skippedFirsts = dropWhile (\l -> not $ "Total Statistics" `isPrefixOf` l) $ lines s
        avgPrefix = "   Avg:"
        stdPrefix = "   Std:"
    let getDoubleFromPrefix prefix
            = (read . drop (length prefix)) <$> find (\l -> prefix `isPrefixOf` l) skippedFirsts
    expavg <- getDoubleFromPrefix avgPrefix
    expstd <- getDoubleFromPrefix stdPrefix
    let getTps = (read . (!! 6) . words) <$> find (\l -> "Run time" `isPrefixOf` l) skippedFirsts
    exptps <- getTps
    return ParsedLog
        { avg = expavg
        , std = expstd
        , tps = exptps
        }
