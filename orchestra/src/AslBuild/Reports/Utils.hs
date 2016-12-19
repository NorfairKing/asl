module AslBuild.Reports.Utils where

import           Data.List

tabularWithHeader :: [String] -> [[String]] -> String
tabularWithHeader header rows =
    tabularWithHeaderAndAllignment (replicate (length header) AllCenter) header rows

tabularWithHeaderAndAllignment :: [TabAll] -> [String] -> [[String]] -> String
tabularWithHeaderAndAllignment _ _ [] = ""
tabularWithHeaderAndAllignment _ _ ([]:_) = ""
tabularWithHeaderAndAllignment alls header ((d:ds):cs) =
    tabularWithAllignment alls $ header : ((("\\hline " ++ d) : ds) : cs)

-- Tabular allignment
data TabAll = AllLeft | AllCenter | AllRight

instance Show TabAll where
    show AllLeft   = "l"
    show AllCenter = "c"
    show AllRight  = "r"

tabular :: [[String]] -> String
tabular rows = tabularWithAllignment (replicate (length rows) AllCenter) rows

tabularWithAllignment :: [TabAll] -> [[String]] -> String
tabularWithAllignment [] _ = ""
tabularWithAllignment alls rows = unlines $
    [ "\\begin{tabular}{|" ++ intercalate "|" (map show alls) ++ "|}"
    ]
    ++ map (tabRow . take (length alls)) rows ++
    [ "\\hline"
    , "\\end{tabular}"
    ]

tabRow :: [String] -> String
tabRow row = "\\hline " ++ intercalate " & " row ++ "\\\\"

