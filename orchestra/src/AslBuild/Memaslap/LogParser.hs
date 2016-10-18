{-# LANGUAGE OverloadedStrings #-}
module AslBuild.Memaslap.LogParser
    ( parseLog
    ) where

import           Control.Monad
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (decimal, double, endOfLine,
                                                   isEndOfLine, space)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as SB

import           Development.Shake

import           AslBuild.Memaslap.Types

parseLog :: FilePath -> Action (Maybe MemaslapLog)
parseLog logFile = do
    need [logFile]
    contents <- liftIO $ SB.readFile logFile
    case parseOnly memaslapLog contents of
        Left err -> do
            putLoud $ "Error parsing log: " ++ err
            return Nothing
        Right res -> return $ Just res

memaslapLog :: Parser MemaslapLog
memaslapLog = do
    header
    trips <- many' statsTriple
    totals <- totalStatsT
    finals <- final
    return MemaslapLog
        { config = ()
        , triples = trips
        , totalStatsTrip = totals
        , finalStats = finals
        }

header :: Parser ()
header = do
    lineThatStartsWith "servers : "
    lineThatStartsWith "threads count: "
    lineThatStartsWith "concurrency: "
    lineThatStartsWith "run time: "
    lineThatStartsWith "windows size: "
    lineThatStartsWith "set proportion: "
    lineThatStartsWith "get proportion: "

lineThatStartsWith :: ByteString -> Parser ()
lineThatStartsWith str = do
    void $ string str
    skipWhile (not . isEndOfLine)
    endOfLine

ansiThingy :: Parser ()
ansiThingy = do
    void $ word8 0x1b
    void $ word8 0x5b
    void $ word8 0x31
    void $ word8 0x3b
    void $ word8 0x31
    void $ word8 0x48
    void $ word8 0x1b
    void $ word8 0x5b
    void $ word8 0x32
    void $ word8 0x4a
    endOfLine

statsTriple :: Parser StatsTriple
statsTriple = do
    ansiThingy
    void $ string "Get Statistics"
    endOfLine
    gl <- statisticsLog
    void $ string "Set Statistics"
    endOfLine
    sl <- statisticsLog
    void $ string "Total Statistics"
    endOfLine
    tl <- statisticsLog
    return StatsTriple
        { getStats = gl
        , setStats = sl
        , bothStats = tl
        }

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
    endOfLine
    return StatisticsLog
        { periodStats = ps
        , globalStats = gs
        }

statistics :: Parser Statistics
statistics = do
    spaces
    t <- decimal
    spaces
    o <- decimal
    spaces
    tp <- decimal
    spaces
    n <- double
    spaces
    g <- decimal
    spaces
    mn <- decimal
    spaces
    mx <- decimal
    spaces
    av <- decimal
    spaces
    st <- double
    spaces
    ge <- double
    endOfLine
    return Statistics
        { time = t
        , ops = o
        , tps = tp
        , net = n
        , getMiss = g
        , minUs = mn
        , maxUs = mx
        , avgUs = av
        , stdDev = st
        , geoDist = ge
        }

spaces :: Parser ()
spaces = void $ many1' space

totalStatsT :: Parser TotalStatsTrip
totalStatsT = do
    void $ string "Get Statistics "
    gts <- totalStats
    void $ string "Set Statistics "
    sts <- totalStats
    void $ string "Total Statistics "
    bts <- totalStats
    return TotalStatsTrip
        { totalGetStats = gts
        , totalSetStats = sts
        , totalBothStats = bts
        }

totalStats :: Parser TotalStats
totalStats = do
    void $ string "("
    es <- decimal
    void space
    void $ string "events)"
    endOfLine
    mn <- titled "Min:" decimal
    mx <- titled "Max:" decimal
    av <- titled "Avg:" decimal
    ge <- titled "Geo:" double
    st <- titled "Std:" double
    spaces
    void $ string "Log2 Dist:"
    endOfLine
    void $ manyTill anyWord8 $ do
        endOfLine -- An empty line
        endOfLine
    return TotalStats
        { totalEvents = es
        , totalMin = mn
        , totalMax = mx
        , totalAvg = av
        , totalGeo = ge
        , totalStd = st
        , totalLog2Dist = ()
        }
  where
    titled :: ByteString -> Parser a -> Parser a
    titled s p = do
        spaces
        void $ string s
        spaces
        r <- p
        endOfLine
        return r


final :: Parser FinalStats
final = do
    cg <- titled "cmd_get:"
    cs <- titled "cmd_set:"
    gm <- titled "get_misses:"
    wb <- titled "written_bytes:"
    rb <- titled "read_bytes:"
    ob <- titled "object_bytes:"
    endOfLine
    void $ string "Run time: "
    void double
    void anyWord8
    spaces
    void $ string "Ops:"
    spaces
    os <- decimal
    spaces
    void $ string "TPS:"
    spaces
    ts <- decimal
    spaces
    void $ string "Net_rate:"
    skipWhile (not . isEndOfLine)
    endOfLine
    return FinalStats
        { finalGets = cg
        , finalSets = cs
        , finalGetMisses = gm
        , finalWrittenBytes = wb
        , finalReadBytes = rb
        , finalObjectBytes = ob
        , finalRuntime = ()
        , finalOps = os
        , finalTps = ts
        , finalNetRate = ()
        }
  where
    titled :: ByteString -> Parser Int
    titled s = do
        void $ string s
        spaces
        r <- decimal
        endOfLine
        return r
