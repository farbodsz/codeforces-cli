--------------------------------------------------------------------------------

module Main where

import Codeforces.Contest
import Codeforces.Rank
import Codeforces.User

import Commands

import Control.Monad.Extra

import Data.List (intercalate)
import Data.Time

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
    putStrLn $ concat [indent, rankName rank, " ", userHandle u]
    whenJust (sequenceA [userFirstName u, userLastName u])
        $ \ns -> putStrLn $ indent ++ unwords ns

    printRatings u
    printPlace u
    putStrLn ""

printRatings :: User -> IO ()
printRatings User {..} = do
    putStrLn ""
    putStrLn $ indent ++ "Rating:       " ++ show userRating
    putStrLn $ indent ++ "              (max: " ++ show userMaxRating ++ ")"

printPlace :: User -> IO ()
printPlace User {..} = do
    whenJust (sequenceA [userCity, userCountry]) $ \xs ->
        putStrLn $ indent ++ "City:         " ++ intercalate ", " xs
    whenJust (userOrganization)
        $ \o -> putStrLn $ indent ++ "Organisation: " ++ o

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
