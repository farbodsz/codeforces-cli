--------------------------------------------------------------------------------

-- | Utility functions for formatting data.
module Codeforces.App.Format where

import Codeforces.Types hiding (RankColor(..))
import qualified Codeforces.Types.Rank as R

import Codeforces.App.Table

import Data.Fixed
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import System.Console.ANSI

--------------------------------------------------------------------------------

-- | 'showText' @x@ is a 'Data.Text' version of 'show'
showText :: Show a => a -> Text
showText = T.pack . show

-- | 'colored' @color text@ wraps some text around SGR codes to display it in
-- the given color.
colored :: Color -> Text -> Text
colored c s = T.concat
    [ T.pack $ setSGRCode [SetColor Foreground Dull c]
    , s
    , T.pack $ setSGRCode [Reset]
    ]

rankColored :: R.RankColor -> Text -> Text
rankColored = colored . convertRankColor

convertRankColor :: R.RankColor -> Color
convertRankColor R.Gray   = White
convertRankColor R.Green  = Green
convertRankColor R.Cyan   = Cyan
convertRankColor R.Blue   = Blue
convertRankColor R.Violet = Magenta
convertRankColor R.Orange = Yellow
convertRankColor R.Red    = Red

-- | Like 'differenceCell' but returns a 'Text' rather than a 'Cell'.
diffColored :: Int -> Text
diffColored x
    | x > 0     = colored Green $ "+" <> showText x
    | x == 0    = " " <> showText x
    | otherwise = colored Red $ showText x

indent :: Text
indent = T.replicate 6 " "

--------------------------------------------------------------------------------

fmtTimeConsumed :: Int -> Text
fmtTimeConsumed x = showText x <> " ms"

fmtMemoryConsumed :: Int -> Text
fmtMemoryConsumed x = showText (x `div` 1000) <> " KB"

-- | Returns an approximate and human-friendly time difference.
--
-- Possible options are:
--  * "just now"
--  * "5 seconds ago"
--  * "X seconds ago" where X is a multiple of 10
--  * "X minutes ago"
--
fmtDiffTime :: NominalDiffTime -> Text
fmtDiffTime diff = go (nominalDiffTimeToSeconds diff)
  where
    go x
        | x < 5     = "just now"
        | x < 10    = "5 seconds ago"
        | x < 60    = showText (x `div'` 10 :: Int) <> "0 seconds ago"
        | otherwise = showText (x `div'` 60 :: Int) <> " minutes ago"

--------------------------------------------------------------------------------

plainCell :: Text -> Cell
plainCell = Cell [Reset]

coloredCell :: Color -> Text -> Cell
coloredCell c = Cell [SetColor Foreground Dull c]

blankCell :: Cell
blankCell = plainCell ""

ratingCell :: Rating -> Cell
ratingCell x =
    let color = convertRankColor $ rankColor $ getRank x
    in coloredCell color (showText x)

-- | 'differenceCell' @diff@ colors a number red, white or green, depending on
-- whether it's negative, 0, or positive.
differenceCell :: Int -> Cell
differenceCell x
    | x > 0     = coloredCell Green $ "+" <> showText x
    | x == 0    = plainCell $ " " <> showText x
    | otherwise = coloredCell Red $ showText x

-- | 'verdictCell' @testset passedTestCount points verdict@ returns a cell
-- displaying the status of a submission, such as "Accepted" or "Wrong answer on
-- pretest 2".
verdictCell :: Testset -> Int -> Maybe Points -> Maybe Verdict -> Cell
verdictCell _       _      _      Nothing  = plainCell "In queue"
verdictCell testset passed points (Just v) = case v of
    Ok -> case testset of
        Tests      -> coloredCell Green "Accepted"
        Samples    -> coloredCell Green "Samples passed"
        Pretests   -> coloredCell Green "Pretests passed"
        Challenges -> coloredCell Green "Challenges passed"
    Partial -> coloredCell Yellow $ maybe
        "Partial result"
        (\pts -> T.concat ["Partial result: ", showText pts, " points"])
        points
    Challenged              -> coloredCell Red "Hacked"
    CompilationError        -> plainCell $ verdictText v
    Skipped                 -> plainCell $ verdictText v
    SecurityViolated        -> plainCell $ verdictText v
    Crashed                 -> plainCell $ verdictText v
    InputPreparationCrashed -> plainCell $ verdictText v
    Rejected                -> plainCell $ verdictText v
    _ ->
        let
            currTest = passed + 1
            clr      = if v == Testing then White else Blue
            text     = T.concat
                [ verdictText v
                , " on "
                , T.toLower . T.init $ showText testset
                , " "
                , showText currTest
                ]
        in coloredCell clr text

--------------------------------------------------------------------------------
