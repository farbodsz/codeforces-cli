--------------------------------------------------------------------------------

module Main where

import Codeforces.Contest
import Codeforces.Problem
import Codeforces.Rank hiding (RankColor(..))
import qualified Codeforces.Rank as R
import Codeforces.RatingChange
import Codeforces.User

import Commands

import Control.Monad.Extra

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import System.Console.ANSI

import Table

--------------------------------------------------------------------------------

main :: IO ()
main = do
    options <- parseCommands
    case options of
        ContestsCmd gym past -> contestList gym past
        ProblemsCmd          -> problemList
        UserCmd    h         -> userInfo h
        RatingsCmd h         -> userRatings h

indent :: Text
indent = T.replicate 6 " "

colored :: Color -> Text -> Text
colored c s = T.concat
    [ T.pack $ setSGRCode [SetColor Foreground Dull c]
    , s
    , T.pack $ setSGRCode [Reset]
    ]

rankColored :: R.RankColor -> Text -> Text
rankColored c = colored (convertRankColor c)

convertRankColor :: R.RankColor -> Color
convertRankColor R.Gray   = White
convertRankColor R.Green  = Green
convertRankColor R.Cyan   = Cyan
convertRankColor R.Blue   = Blue
convertRankColor R.Violet = Magenta
convertRankColor R.Orange = Yellow
convertRankColor R.Red    = Red

plainCell :: Text -> Cell
plainCell = Cell [Reset]

coloredCell :: Color -> Text -> Cell
coloredCell c = Cell [SetColor Foreground Dull c]

--------------------------------------------------------------------------------

contestList :: Bool -> Bool -> IO ()
contestList gym past = do
    contests <- getContests gym
    now      <- getCurrentTime
    either print (printContests . filterContests past now) contests

-- | `filterContests` @onlyPast currentTime@ filters and orders a list of
-- contests depending on whether past or upcoming should be displayed.
filterContests :: Bool -> UTCTime -> [Contest] -> [Contest]
filterContests past now = if past
    then filter isContestPast
    else reverse . filter (not . isContestPast)
  where
    isContestPast :: Contest -> Bool
    isContestPast c = case contestStartTime c of
        Nothing -> False
        Just t  -> t < now

printContests :: [Contest] -> IO ()
printContests cs = forM_ (makeTable headers rows) T.putStrLn
  where
    headers = [("#", 4), ("Name", 50), ("Date", 16), ("Duration", 10)]
    rows    = map
        (\Contest {..} ->
            plainCell
                <$> [ T.pack $ show contestId
                    , contestName
                    , fmtStartTime contestStartTime
                    , fmtDuration contestDuration
                    ]
        )
        cs

fmtStartTime :: Maybe UTCTime -> Text
fmtStartTime = \case
    Nothing -> " "
    Just t  -> T.pack $ formatTime defaultTimeLocale "%H:%M  %d-%b-%y" t

fmtDuration :: DiffTime -> Text
fmtDuration = T.pack . formatTime defaultTimeLocale "%h:%0M hrs"

--------------------------------------------------------------------------------

problemList :: IO ()
problemList = getProblems [] >>= either print printProblems

printProblems :: [Problem] -> IO ()
printProblems ps = forM_ (makeTable headers rows) T.putStrLn
  where
    headers = [("#", 6), ("Name", 40), ("Rating", 6)]
    rows    = map
        (\Problem {..} ->
            plainCell
                <$> [ maybe "" (T.pack . show) problemContestId <> problemIndex
                    , problemName
                    , maybe "" (T.pack . show) problemRating
                    ]
        )
        ps

--------------------------------------------------------------------------------

userInfo :: Handle -> IO ()
userInfo h = getUser h >>= either print printUser

printUser :: User -> IO ()
printUser u = do
    let rank = getRank (userRating u)

    putStrLn ""
    T.putStrLn $ rankColored (rankColor rank) $ T.concat
        [indent, rankName rank, " ", userHandle u, "\n"]
    whenJust (sequenceA [userFirstName u, userLastName u])
        $ \ns -> T.putStrLn $ indent <> T.unwords ns

    printRatings u
    printPlace u
    putStrLn ""

printRatings :: User -> IO ()
printRatings User {..} = do
    putStrLn ""
    T.putStrLn $ T.concat
        [ indent
        , "Rating:       "
        , rankColored (rankColor (getRank userRating)) (T.pack $ show userRating)
        ]
    let maxRank = getRank userRating
    T.putStrLn $ T.concat
        [ indent
        , "              (max: "
        , rankColored
            (rankColor maxRank)
            (T.concat [rankName maxRank, ", ", T.pack $ show userMaxRating])
        , ")"
        ]

printPlace :: User -> IO ()
printPlace User {..} = do
    whenJust (sequenceA [userCity, userCountry]) $ \xs ->
        T.putStrLn $ indent <> "City:         " <> T.intercalate ", " xs
    whenJust userOrganization $ \o -> T.putStrLn $ indent <> "Organisation: " <> o

--------------------------------------------------------------------------------

userRatings :: Handle -> IO ()
userRatings h = getUserRatingHistory h >>= either print printRatingChanges

printRatingChanges :: [RatingChange] -> IO ()
printRatingChanges rcs = forM_ (makeTable headers rows) T.putStrLn
  where
    headers =
        [("#", 3), ("Contest", 50), ("Rank", 5), ("Change", 6), ("Rating", 6)]
    rows = reverse $ zipWith
        (\RatingChange {..} num ->
            [ plainCell $ T.pack $ show num
            , plainCell rcContestName
            , plainCell $ T.pack $ show rcRank
            , fmtChange (rcNewRating - rcOldRating)
            , fmtRating rcNewRating
            ]
        )
        rcs
        ([1 ..] :: [Int])

    fmtChange :: Int -> Cell
    fmtChange x
        | x > 0     = coloredCell Green $ T.concat ["+", T.pack $ show x]
        | x == 0    = plainCell $ T.concat [" ", T.pack $ show x]
        | otherwise = coloredCell Red $ T.pack $ show x

    fmtRating :: Int -> Cell
    fmtRating x =
        let color = convertRankColor $ rankColor $ getRank x
        in coloredCell color $ T.pack $ show x

--------------------------------------------------------------------------------
