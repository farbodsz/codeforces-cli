--------------------------------------------------------------------------------

module Codeforces.App.Table
    ( Cell(..)
    , Row
    , Table
    , makeTable
    ) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           System.Console.ANSI

--------------------------------------------------------------------------------

-- | A cell of a table consists of SGR styles and some text content
data Cell = Cell [SGR] Text

cellToText :: Cell -> Text
cellToText (Cell sgrs x) =
    T.concat [T.pack $ setSGRCode sgrs, x, T.pack $ setSGRCode [Reset]]

--------------------------------------------------------------------------------

-- | Name and width of a column
type ColConfig = (Text, Int)

type Row = [Cell]

-- | The table output is a list of row strings.
type Table = [Text]

-- | `makeTable` @colConfigs rows@ returns a list of row strings including the
-- header row
makeTable :: [ColConfig] -> [Row] -> Table
makeTable hs rs = makeHeader hs : map (makeRow hs) rs

makeHeader :: [ColConfig] -> Text
makeHeader hs = T.intercalate colSep $ map (\(s, w) -> pad w s) hs

-- | `makeRow` @colConfigs row@ makes a row string from the supplied column
-- widths and row cells.
makeRow :: [ColConfig] -> Row -> Text
makeRow hs row = T.intercalate colSep paddedCells
  where
    paddedCells = zipWith (\(_, w) cell -> fmtCell w cell) hs row
    fmtCell w (Cell sgrs x) = cellToText $ Cell sgrs (pad w x)

colSep :: Text
colSep = "  "

pad :: Int -> Text -> Text
pad w s | len > w   = T.take (w - 2) s <> ".."
        | otherwise = s <> T.replicate (w - len) " "
    where len = T.length s

--------------------------------------------------------------------------------
