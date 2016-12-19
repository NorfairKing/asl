module AslBuild.Reports.SignTable where

import           Control.Monad
import           Data.List
import           Text.Printf

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Analysis.Memaslap
import           AslBuild.Analysis.Types
import           AslBuild.Constants
import           AslBuild.Experiment
import           AslBuild.Experiments.Factorial
import           AslBuild.Experiments.Factorial.Types
import           AslBuild.Reports.Common
import           AslBuild.Reports.Utils
import           AslBuild.Utils


signTableRule :: String
signTableRule = "sign-table"

signTableRules :: Rules ()
signTableRules = do
    rs <- mapM signTableRulesFor allFactorialExperiments
    signTableRule ~> need rs

signTableRuleFor :: ExperimentConfig a => a -> String
signTableRuleFor ecf = experimentTarget ecf ++ "-sign-table"

signTableRulesFor :: FactorialCfg -> Rules String
signTableRulesFor ecf = do
    signTableFiles ecf &%> \_ -> do
        genTpsSignTable ecf
        genRespSignTable ecf


    let thisTarget = signTableRuleFor ecf
    thisTarget ~> need (signTableFiles ecf)
    return thisTarget

signTablePrefix :: ExperimentConfig a => a -> FilePath
signTablePrefix ecf = reportsTmpDir </> experimentTarget ecf ++ "-sign-table-" <.> texExt

signTableFileWithPostfix :: ExperimentConfig a => a -> String -> FilePath
signTableFileWithPostfix ecf postfix = changeFilename (++ postfix) $ signTablePrefix ecf

signTableFiles :: ExperimentConfig a => a -> [FilePath]
signTableFiles ecf = do
    measure <- ["tps", "resp"]
    extra <- ["results", "legend"] ++ [intercalate "-" [a, b] | a <- ["add", "mul"], b <- ["signtable", "variations"]]
    pure $ signTableFileWithPostfix ecf $ intercalate "-" [measure, extra]

signTableFileForReport :: FilePath -> Int -> FilePath
signTableFileForReport file i = file `replaceDirectory` reportGenfileDir i

useSignTableInReport :: ExperimentConfig a => a -> Int -> Rules ()
useSignTableInReport ecf i = forM_ (signTableFiles ecf) $ \eff ->
    signTableFileForReport eff i `byCopying` eff

dependOnSignTableForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnSignTableForReport ecf i = need $ map (`signTableFileForReport` i) $ signTableFiles ecf

genTpsSignTable :: FactorialCfg -> Action ()
genTpsSignTable = genSignTableWith
    tpsResults
    avgTpsResults
    id
    "tps"
    "Throughput (transactions / second)"

genRespSignTable :: FactorialCfg -> Action ()
genRespSignTable = genSignTableWith
    respResults
    avgRespResults
    (/100)
    "resp"
    "Response Time ($100 \\mu s$)"

genSignTableWith
    :: (MemaslapClientResults -> AvgResults)
    -> (CombinedClientResults -> MetaAvgResults)
    -> (Double -> Double)
    -> String
    -> String
    -> FactorialCfg
    -> Action ()
genSignTableWith funcRes funcAvgRes convFunc measSuf measure ecf = do
    genLegendTable measure measSuf ecf
    genResultsTable funcRes funcAvgRes convFunc measSuf ecf

    let go = genSingleSignTable funcRes funcAvgRes convFunc measSuf ecf
    go id id "add"
    go (logBase 10) (\x -> 10 ** x) "mul"

genLegendTable
    :: ExperimentConfig a
    => String -> String -> a -> Action ()
genLegendTable measure measSuf ecf = do
    -- Reverse order from where they're drawn from the list
    let legendTable = tabular
            [ ["A", "Replication coefficient"]
            , ["B", "Request Value Size"]
            , ["C", "Write Percentage"]
            , ["y", measure]
            ]

    let legendFile = signTableFileWithPostfix ecf $ measSuf ++ "-legend"
    writeFile' legendFile legendTable

genResultsTable
    :: (MemaslapClientResults -> AvgResults)
    -> (CombinedClientResults -> MetaAvgResults)
    -> (Double -> Double)
    -> String
    -> FactorialCfg
    -> Action ()
genResultsTable funcRes funcAvgRes convFunc measSuf ecf = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    let choices = [-1, 1] :: [Int]
    let tot = 8 :: Int
    let veci = replicate tot 1 :: [Int]
    let vecta = do
            void choices
            void choices
            r <- choices
            pure r
    let vectb = do
            void choices
            r <- choices
            void choices
            pure r
    let vectc = do
            r <- choices
            void choices
            void choices
            pure r

    let headerPrefix = sortOn length $ do
            c <- ["", "C"]
            b <- ["", "B"]
            a <- ["", "A"]
            let abc = a ++ b ++ c
            pure $ if abc == "" then "I" else abc

    let mult :: Num a => [a] -> [a] -> [a]
        mult v1 v2 = zipWith (*) v1 v2

    let columnFor :: String -> [Int]
        columnFor = foldr go veci
          where
            go 'I' = mult veci
            go 'A' = mult vecta
            go 'B' = mult vectb
            go 'C' = mult vectc
            go _   = error "must not happen."

    let roundD = round :: (Double -> Integer)
    realRes <- forM slocss $ \slocs -> do
        let combinedResF = combinedClientRepsetResultsFile ecf slocs
            resFs = map (combineClientResultsFile ecf) slocs
        need $ combinedResF : resFs
        cr <- readCombinedClientsResults combinedResF
        ress <- mapM readCombinedClientResults resFs
        let individuals = map (convFunc . avg . bothResults . funcRes) ress
            res = convFunc $ avgAvgs $ avgBothResults $ funcAvgRes cr
        pure (individuals, res)
    let indivHeader = take 3 (drop 1 headerPrefix)
    let resultsHeader = indivHeader ++ ["$y$", "$\\bar{y}$"]
    let resultsRows = flip map realRes $ \(indivs, res) -> [show $ map roundD indivs, printf "%.2f" res]
    let resultsTable = tabularWithHeaderAndAllignment [AllCenter, AllCenter, AllCenter, AllCenter, AllRight]
            resultsHeader
            (zipWith (++) (map (map show) $ transpose $ map columnFor indivHeader) resultsRows)

    let resultsFile = signTableFileWithPostfix ecf $ measSuf ++ "-results"
    writeFile' resultsFile resultsTable

genSingleSignTable
    :: (MemaslapClientResults -> AvgResults)
    -> (CombinedClientResults -> MetaAvgResults)
    -> (Double -> Double)
    -> String
    -> FactorialCfg
    -> (Double -> Double)
    -> (Double -> Double)
    -> String
    -> Action ()
genSingleSignTable funcRes funcAvgRes convFunc measSuf ecf _ _ suffix = do
    slocss <- readResultsSummaryLocationsForCfg ecf
    let choices = [-1, 1] :: [Int]
    let tot = 8 :: Int
        reps = repititions $ hlConfig ecf
    let veci = replicate tot 1 :: [Int]
    let vecta = do
            void choices
            void choices
            r <- choices
            pure r
    let vectb = do
            void choices
            r <- choices
            void choices
            pure r
    let vectc = do
            r <- choices
            void choices
            void choices
            pure r

    let headerPrefix = sortOn length $ do
            c <- ["", "C"]
            b <- ["", "B"]
            a <- ["", "A"]
            let abc = a ++ b ++ c
            pure $ if abc == "" then "I" else abc

    let mult :: Num a => [a] -> [a] -> [a]
        mult v1 v2 = zipWith (*) v1 v2

    let columnFor :: String -> [Int]
        columnFor = foldr go veci
          where
            go 'I' = mult veci
            go 'A' = mult vecta
            go 'B' = mult vectb
            go 'C' = mult vectc
            go _   = error "must not happen."

    let signColumns :: [[Int]]
        signColumns = map columnFor headerPrefix
        signRows :: [[Int]]
        signRows = transpose signColumns
        signRowSs = map (map show) signRows

    let roundD = round :: (Double -> Integer)
    realRes <- forM slocss $ \slocs -> do
        let combinedResF = combinedClientRepsetResultsFile ecf slocs
            resFs = map (combineClientResultsFile ecf) slocs
        need $ combinedResF : resFs
        cr <- readCombinedClientsResults combinedResF
        ress <- mapM readCombinedClientResults resFs
        let individuals = map (convFunc . avg . bothResults . funcRes) ress
            res = convFunc $ avgAvgs $ avgBothResults $ funcAvgRes cr
        pure (individuals, res)

    let tups = map (\(indivs, res) -> (indivs, res)) realRes

    let fullResVec = map fst tups :: [[Double]]
        resvec = map snd tups :: [Double]

    let effects :: [Double]
        effects = flip map signColumns $ \signColumn     ->
            sum $ mult (map fromIntegral signColumn) resvec
        effDivs :: [Double]
        effDivs = map (/ fromIntegral tot) effects

    let secondToLastRow = map (show . roundD) effects ++ ["Total", ""]
    let effectRow = map (show . roundD) effDivs ++ ["Total/8", ""]
    let effectRows = [secondToLastRow, effectRow]

    let preds = flip map signRows $ \signRow -> sum $ mult (map fromIntegral signRow) effDivs

    let errss :: [[Double]]
        errss = flip map (zip fullResVec preds) $ \(ress_, pred_) -> map (\res -> res - pred_) ress_


    let rows = flip map (zip (zip signRowSs tups) errss) $ \((row, (_, res)), errs) ->
            let resS = show $ roundD res
                errsS = show $ map roundD errs
            in row ++ [resS, errsS]

    -- let meanRes = S.mean $ V.fromList resvec
    let sqs = concatMap (map (** 2)) fullResVec
        totR = fromIntegral $ tot * reps :: Double
        sqEffDiff = map (** 2) effDivs
        ss0 = totR * head sqEffDiff
        ssy = sum sqs
        sse = ssy - totR * sum sqEffDiff
        sst = ssy - ss0

    let variations = map ((* totR) . (** 2)) $ drop 1 effDivs
        relvars = map (/ sst) variations

    let showPerc = (printf "%.2f" :: Double -> String) . (*100)

    let headerRest = ["$\\bar{y}$", "Err"]
        header = headerPrefix ++ headerRest
        table = rows ++ effectRows

    let signTable = tabularWithHeader
            header
            table

    let tableFile = signTableFileWithPostfix ecf $ intercalate "-" [measSuf, suffix, "signtable"]
    writeFile' tableFile signTable

    let predTable = tabularWithHeaderAndAllignment [AllCenter, AllRight, AllRight]
            ["Factor", "Variation", "Percentage"]
            (transpose [drop 1 headerPrefix ++ ["Error"], map (show . roundD) (variations ++ [sse]), map showPerc (relvars ++ [sse/sst])])

    let predictionFile = signTableFileWithPostfix ecf $ intercalate "-" [measSuf, suffix, "variations"]
    writeFile' predictionFile predTable
