module Table
    ( makeTable
    ) where

import Data.List (intercalate)


-- | Name and width of a column
type ColConfig = (String, Int)

type Row = [String]

-- | `makeTable` @colConfigs rows@ returns a list of row strings including the
-- header row
makeTable :: [ColConfig] -> [Row] -> [String]
makeTable hs rs = makeHeader hs : map (makeRow hs) rs

makeHeader :: [ColConfig] -> String
makeHeader hs = intercalate colSep $ map (\(s, w) -> pad w s) hs

-- | `makeRow` @colConfigs row@ makes a row string from the supplied column
-- widths and row cells.
makeRow :: [ColConfig] -> Row -> String
makeRow hs row = intercalate colSep paddedCells
    where paddedCells = zipWith (\(_, w) cell -> pad w cell) hs row

colSep :: String
colSep = "  "

pad :: Int -> String -> String
pad w s
    | len > w   = take (w - 2) s ++ ".."
    | otherwise = s ++ replicate (w - len) ' '
    where len = length s
