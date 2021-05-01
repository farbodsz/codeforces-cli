--------------------------------------------------------------------------------

module Main where

import Codeforces.User

import Commands

--------------------------------------------------------------------------------

main :: IO ()
main = do
    options <- parseCommands

    case options of
        UserCmd h -> userInfo h

--------------------------------------------------------------------------------

userInfo :: Handle -> IO ()
userInfo h = do
    usr <- getUser h
    case usr of
        Left  e -> print e
        Right u -> printUser u

printUser :: User -> IO ()
printUser = print

--------------------------------------------------------------------------------
