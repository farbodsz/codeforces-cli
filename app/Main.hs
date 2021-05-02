--------------------------------------------------------------------------------

module Main where

import Codeforces.Contest
import Codeforces.Rank hiding (RankColor(..))
import qualified Codeforces.Rank as R
import Codeforces.User

import Commands

import Control.Monad.Extra

import Data.List (intercalate)
import Data.Time

import System.Console.ANSI

import Table

--------------------------------------------------------------------------------

main :: IO ()
main = do
    options <- parseCommands

    case options of
        ContestsCmd gym past -> contestList gym past
        UserCmd h            -> userInfo h

--------------------------------------------------------------------------------

indent :: String
indent = replicate 6 ' '

putRankStr :: R.RankColor -> String -> IO ()
putRankStr c s = do
    setSGR [SetColor Foreground Dull (convertRankColor c)]
    putStr s
    setSGR [Reset]

convertRankColor :: R.RankColor -> Color
convertRankColor R.Gray   = White
convertRankColor R.Green  = Green
convertRankColor R.Cyan   = Cyan
convertRankColor R.Blue   = Blue
convertRankColor R.Violet = Magenta
convertRankColor R.Orange = Yellow
convertRankColor R.Red    = Red

--------------------------------------------------------------------------------

userInfo :: Handle -> IO ()
userInfo h = do
    usr <- getUser h
    case usr of
        Left  e -> print e
        Right u -> printUser u

printUser :: User -> IO ()
printUser u = do
    let rank = getRank (userRating u)

    putStrLn ""
    putRankStr (rankColor rank)
        $ concat [indent, rankName rank, " ", userHandle u, "\n"]
    whenJust (sequenceA [userFirstName u, userLastName u])
        $ \ns -> putStrLn $ indent ++ unwords ns

    printRatings u
    printPlace u
    putStrLn ""

printRatings :: User -> IO ()
printRatings User {..} = do
    putStrLn ""
    let currRank = getRank userRating
    putStr $ indent ++ "Rating:       "
    putRankStr (rankColor currRank) $ show userRating ++ "\n"

    let maxRank = getRank userRating
    putStr $ indent ++ "              (max: "
    putRankStr (rankColor maxRank)
        $ concat [rankName maxRank, ", ", show userMaxRating]
    putStrLn ")"

printPlace :: User -> IO ()
printPlace User {..} = do
    whenJust (sequenceA [userCity, userCountry]) $ \xs ->
        putStrLn $ indent ++ "City:         " ++ intercalate ", " xs
    whenJust userOrganization $ \o -> putStrLn $ indent ++ "Organisation: " ++ o

--------------------------------------------------------------------------------

contestList :: Bool -> Bool -> IO ()
contestList gym past = do
    contests <- getContests gym
    case contests of
        Left  e  -> print e
        Right cs -> do
            now <- getCurrentTime
            printContests $ filterContests past now cs

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
printContests cs = forM_ (makeTable headers rows) putStrLn
  where
    headers = [("#", 4), ("Name", 50), ("Date", 16), ("Duration", 10)]
    rows    = map
        (\Contest {..} ->
            [ show contestId
            , contestName
            , fmtStartTime contestStartTime
            , fmtDuration contestDuration
            ]
        )
        cs

fmtStartTime :: Maybe UTCTime -> String
fmtStartTime = \case
    Nothing -> " "
    Just t  -> formatTime defaultTimeLocale "%H:%M  %d-%b-%y" t

fmtDuration :: DiffTime -> String
fmtDuration = formatTime defaultTimeLocale "%h:%0M hrs"

--------------------------------------------------------------------------------
