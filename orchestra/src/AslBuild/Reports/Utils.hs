module AslBuild.Reports.Utils where

import           Data.List

tabularWithHeader :: [String] -> [[String]] -> String
tabularWithHeader _ [] = ""
tabularWithHeader _ ([]:_) = ""
tabularWithHeader header ((d:ds):cs) =
    tabular $ header : ((("\\hline " ++ d) : ds) : cs)

tabular :: [[String]] -> String
tabular [] = ""
tabular rows = unlines $
    [ "\\begin{tabular}{" ++ concat (replicate (length (head rows)) "|c") ++ "|}"
    ]
    ++ map tabRow rows ++
    [ "\\hline"
    , "\\end{tabular}"
    ]

tabRow :: [String] -> String
tabRow row = "\\hline " ++ intercalate " & " row ++ "\\\\"

