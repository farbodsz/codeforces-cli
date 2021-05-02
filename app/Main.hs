--------------------------------------------------------------------------------

module Main where

import Codeforces.Rank
import Codeforces.User

import Commands

import Control.Monad.Extra

import Data.List (intercalate)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    options <- parseCommands

    case options of
        UserCmd h -> userInfo h

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
    let rank = getRank (rating u)

    putStrLn ""
    putStrLn $ concat [indent, rankName rank, " ", handle u]
    whenJust (sequenceA [firstName u, lastName u])
        $ \ns -> putStrLn $ indent ++ unwords ns

    printRatings u
    printPlace u
    putStrLn ""

printRatings :: User -> IO ()
printRatings u = do
    let currRating = rating u
    let bestRating = maxRating u
    putStrLn ""
    putStrLn $ indent ++ "Rating:       " ++ show currRating
    putStrLn $ indent ++ "              (max: " ++ show bestRating ++ ")"

printPlace :: User -> IO ()
printPlace u = do
    whenJust (sequenceA [city u, country u]) $ \xs ->
        putStrLn $ indent ++ "City:         " ++ intercalate ", " xs
    whenJust (organization u) $ \o -> putStrLn $ indent ++ "Organisation: " ++ o

--------------------------------------------------------------------------------
