--------------------------------------------------------------------------------

module Table
    ( Cell(..)
    , makeTable
    ) where

import Data.List (intercalate)

import System.Console.ANSI

--------------------------------------------------------------------------------

-- | A cell of a table consists of SGR styles and some string content
data Cell = Cell [SGR] String

cellToString :: Cell -> String
cellToString (Cell sgrs x) = concat [setSGRCode sgrs, x, setSGRCode [Reset]]

--------------------------------------------------------------------------------

-- | Name and width of a column
type ColConfig = (String, Int)

type Row = [Cell]

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
  where
    paddedCells = zipWith (\(_, w) cell -> fmtCell w cell) hs row
    fmtCell w (Cell sgrs x) = cellToString $ Cell sgrs (pad w x)

colSep :: String
colSep = "  "

pad :: Int -> String -> String
pad w s
    | len > w   = take (w - 2) s ++ ".."
    | otherwise = s ++ replicate (w - len) ' '
    where len = length s

--------------------------------------------------------------------------------
