module AslBuild.Reports.SignTable where

import           Control.Monad
import           Data.List

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
    let tpsSignTableF = signTableTpsFile ecf
        respSignTableF = signTableRespFile ecf
    tpsSignTableF %> \_ -> genTpsSignTable ecf >>= writeFile' tpsSignTableF
    respSignTableF %> \_ -> genRespSignTable ecf >>= writeFile' respSignTableF

    let thisTarget = signTableRuleFor ecf
    thisTarget ~> need [tpsSignTableF, respSignTableF]
    return thisTarget

signTableTpsFile :: ExperimentConfig a => a -> FilePath
signTableTpsFile ecf = reportsTmpDir </> experimentTarget ecf ++ "-sign-table-tps" <.> texExt

signTableRespFile :: ExperimentConfig a => a -> FilePath
signTableRespFile ecf = reportsTmpDir </> experimentTarget ecf ++ "-sign-table-resp" <.> texExt

signTableFiles :: ExperimentConfig a => a -> [FilePath]
signTableFiles ecf = [signTableTpsFile ecf, signTableRespFile ecf]

signTableFileForReport :: FilePath -> Int -> FilePath
signTableFileForReport file i = file `replaceDirectory` reportGenfileDir i

useSignTableInReport :: ExperimentConfig a => a -> Int -> Rules ()
useSignTableInReport ecf i = forM_ (signTableFiles ecf) $ \eff ->
    signTableFileForReport eff i `byCopying` eff

dependOnSignTableForReport :: ExperimentConfig a => a -> Int -> Action ()
dependOnSignTableForReport ecf i = need $ map (`signTableFileForReport` i) $ signTableFiles ecf

genTpsSignTable :: FactorialCfg -> Action String
genTpsSignTable = genSignTableWith tpsResults avgTpsResults "Throughput (transactions / second)"

genRespSignTable :: FactorialCfg -> Action String
genRespSignTable = genSignTableWith respResults avgRespResults "Response Time ($\\mu s$)"

genSignTableWith
    :: (MemaslapClientResults -> AvgResults)
    -> (CombinedClientResults -> MetaAvgResults)
    -> String
    -> FactorialCfg
    -> Action String
genSignTableWith funcRes funcAvgRes measure ecf = do
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
            go _ = error "must not happen."

        signRows = map columnFor headerPrefix
        signRowSs = map (map show) $ transpose signRows

    let roundD = round :: (Double -> Integer)
    tups <- forM slocss $ \slocs -> do
        let combinedResF = combinedClientRepsetResultsFile ecf slocs
            resFs = map (combineClientResultsFile ecf) slocs
        need $ combinedResF : resFs
        cr <- readCombinedClientsResults combinedResF
        ress <- mapM readCombinedClientResults resFs
        let individuals = map (avg . bothResults . funcRes) ress
            res = avgAvgs $ avgBothResults $ funcAvgRes cr
        pure (individuals, res)

    let rows = flip map (zip signRowSs tups) $ \(row, (individuals, res)) ->
            let individualsS = show $ map roundD individuals
                resS = show $ roundD res
            in row ++ [individualsS, resS]

    let resvec = map snd tups :: [Double]
    let reasons :: [Double]
        reasons = flip map signRows $ \signRow ->
            sum $ mult (map fromIntegral signRow) resvec
    let secondToLastRow = map (show . roundD) reasons ++ ["", "Total"]
    let lastRow = map (show . roundD . (/ fromIntegral tot)) reasons ++ ["", "Total/" ++ show tot]
    let lastRows = [secondToLastRow, lastRow]


    let headerRest = ["y", "$\\bar{y}$"]
        header = headerPrefix ++ headerRest
        table = rows ++ lastRows

    let signTable = tabularWithHeader
            header
            table

    let legendTable = tabular
            [ ["A", "Value size"]
            , ["B", "Key Size"]
            , ["C", "Virtual clients"]
            , ["y", measure]
            ]
    pure $ unlines [legendTable, signTable]
