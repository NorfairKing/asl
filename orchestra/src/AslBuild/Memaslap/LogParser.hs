{-# LANGUAGE OverloadedStrings #-}

module AslBuild.Memaslap.LogParser
    ( parseLog
    ) where

import Control.Monad
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

import Development.Shake

import AslBuild.Memaslap.Types

parseLog :: FilePath -> Action (Maybe MemaslapLog)
parseLog logFile = do
    need [logFile]
    eer <- liftIO $ parseFromFile memaslapLog logFile
    case eer of
        Left err -> do
            putLoud $ "Error parsing log: " ++ show err
            return Nothing
        Right res -> return $ Just res

memaslapLog :: Parser MemaslapLog
memaslapLog = do
    header
    trips <- many statsTriple
    totals <- option Nothing $ Just <$> totalStatsT
    finals <- final
    return MemaslapLog {config = (), triples = trips, totalStatsTrip = totals, finalStats = finals}

header :: Parser ()
header = do
    lineThatStartsWith "servers : "
    lineThatStartsWith "threads count: "
    lineThatStartsWith "concurrency: "
    lineThatStartsWith "run time: "
    lineThatStartsWith "windows size: "
    lineThatStartsWith "set proportion: "
    lineThatStartsWith "get proportion: "

lineThatStartsWith :: String -> Parser ()
lineThatStartsWith str = do
    void $ string str
    void $ anyChar `manyTill` try endOfLine

ansiThingy :: Parser ()
ansiThingy = do
    void $ char '\ESC'
    void $ char '['
    void $ char '1'
    void $ char ';'
    void $ char '1'
    void $ char 'H'
    void $ char '\ESC'
    void $ char '['
    void $ char '2'
    void $ char 'J'
    void endOfLine

statsTriple :: Parser StatsTriple
statsTriple = do
    ansiThingy
    gl <-
        optionMaybe $
        try $ do
            void $ string "Get Statistics"
            void endOfLine
            statisticsLog
    sl <-
        optionMaybe $
        try $ do
            void $ string "Set Statistics"
            void endOfLine
            statisticsLog
    void $ string "Total Statistics"
    void endOfLine
    tl <- statisticsLog
    return StatsTriple {getStats = gl, setStats = sl, bothStats = tl}

statisticsLog :: Parser StatisticsLog
statisticsLog = do
    void $ string "Type"
    spaces
    void $ string "Time(s)"
    spaces
    void $ string "Ops"
    spaces
    void $ string "TPS(ops/s)"
    spaces
    void $ string "Net(M/s)"
    spaces
    void $ string "Get_miss"
    spaces
    void $ string "Min(us)"
    spaces
    void $ string "Max(us)"
    spaces
    void $ string "Avg(us)"
    spaces
    void $ string "Std_dev"
    spaces
    void $ string "Geo_dist"
    spaces
    void $ string "Period"
    ps <- statistics
    void $ string "Global"
    gs <- statistics
    return StatisticsLog {periodStats = ps, globalStats = gs}

statistics :: Parser Statistics
statistics = do
    t <- integer
    o <- integer
    tp <- integer
    n <- double
    g <- integer
    mn <- integer
    mx <- integer
    av <- integer
    st <- doubleOrNan
    ge <- doubleOrNan
    return
        Statistics
        { time = t
        , ops = o
        , tps = tp
        , net = n
        , getMiss = g
        , minUs = mn
        , maxUs = mx
        , avgUs = av
        , std = st
        , geoDist = ge
        }

doubleOrNan :: Parser Double
doubleOrNan =
    try (string "nan" >> spaces >> return (-1)) <|> try (string "-nan" >> spaces >> return (-1)) <|>
    double

totalStatsT :: Parser TotalStatsTrip
totalStatsT = do
    gts <-
        optionMaybe $
        try $ do
            void $ string "Get Statistics "
            totalStats
    sts <-
        optionMaybe $
        try $ do
            void $ string "Set Statistics "
            totalStats
    void $ string "Total Statistics "
    bts <- totalStats
    return TotalStatsTrip {totalGetStats = gts, totalSetStats = sts, totalBothStats = bts}

totalStats :: Parser TotalStats
totalStats = do
    void $ string "("
    es <- integer
    void $ string "events)"
    void endOfLine
    mn <- titled "Min:" integer
    mx <- titled "Max:" integer
    av <- titled "Avg:" integer
    ge <- titled "Geo:" double
    st <- titled "Std:" double
    spaces
    void $ string "Log2 Dist:"
    void endOfLine
    void $
        manyTill anyChar $
        try $ do
            void endOfLine -- An empty line
            void endOfLine
    return
        TotalStats
        { totalEvents = es
        , totalMin = mn
        , totalMax = mx
        , totalAvg = av
        , totalGeo = ge
        , totalStd = st
        , totalLog2Dist = ()
        }
  where
    titled :: String -> Parser a -> Parser a
    titled s p = do
        spaces
        void $ string s
        spaces
        p

final :: Parser FinalStats
final = do
    cg <- titled "cmd_get:"
    cs <- titled "cmd_set:"
    gm <- titled "get_misses:"
    wb <- titled "written_bytes:"
    rb <- titled "read_bytes:"
    ob <- titled "object_bytes:"
    rt <-
        do void $ string "Run time: "
           v <- double
           void $ char 's'
           return v
    spaces
    void $ string "Ops:"
    spaces
    os <- integer
    spaces
    void $ string "TPS:"
    spaces
    ts <- integer
    spaces
    void $ string "Net_rate:"
    void $ anyChar `manyTill` try endOfLine
    return
        FinalStats
        { finalGets = cg
        , finalSets = cs
        , finalGetMisses = gm
        , finalWrittenBytes = wb
        , finalReadBytes = rb
        , finalObjectBytes = ob
        , finalRuntime = rt
        , finalOps = os
        , finalTps = ts
        , finalNetRate = ()
        }
  where
    titled :: String -> Parser Int
    titled s = do
        void $ string s
        void space
        integer

integer
    :: Integral a
    => Parser a
integer = fromIntegral <$> Token.integer (Token.makeTokenParser emptyDef)

double :: Parser Double
double = Token.float $ Token.makeTokenParser emptyDef
-- stop :: Parser a
-- stop = do
--     rest <- getInput
--     error $ show rest
