module Main where

import Codeforces.User

main :: IO ()
main = getUsers ["tourist"] >>= print
