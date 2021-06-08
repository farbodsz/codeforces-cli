--------------------------------------------------------------------------------

module Watcher where

import Codeforces

import Control.Concurrent
import Control.Monad (zipWithM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import qualified Data.Text.IO as T

import System.Console.ANSI

import Table

--------------------------------------------------------------------------------

-- | 'handleWatch' @shouldWatch m@ runs computation @m@ once if @shouldWatch@ is
-- false, otherwise 'watchTable' watches it.
handleWatch :: Bool -> IO (Either CodeforcesError Table) -> IO ()
handleWatch False m = m >>= either (putStrLn . showE) (mapM_ T.putStrLn)
handleWatch True  m = evalStateT (watchTable 5 m) []

-- | 'watchTable' @delaySecs m@ runs computation @m@ every @delaySecs@ amount of
-- seconds. The terminal output from @m@ is changed if the next run of @m@
-- yields a different result.
watchTable :: Int -> IO (Either CodeforcesError Table) -> StateT Table IO ()
watchTable delaySecs m = do
    currTable <- get
    result    <- lift m

    case result of
        Left  e         -> lift $ putStrLn $ showE e
        Right nextTable -> if currTable == nextTable
            then pure ()
            else do
                lift $ updateTableOutput currTable nextTable
                put nextTable

    lift $ threadDelay (delaySecs * 1000000)
    watchTable delaySecs m

-- | Takes the current and new tables, and updates any rows in the existing
-- terminal output that have changed.
updateTableOutput :: Table -> Table -> IO ()
updateTableOutput currTable nextTable = do
    setCursorColumn 0
    cursorUpLine (length currTable)
    -- Tables need same num of rows, else zip will stop after the smallest table
    let ts = pad2 currTable nextTable
    uncurry (zipWithM_ updateRow) ts
  where
    updateRow r1 r2
        | r1 == r2  = cursorDownLine 1
        | otherwise = T.putStrLn r2

--------------------------------------------------------------------------------

-- | Takes 2 lists and makes them both the same length by adding padding.
-- The padding character used is 'mempty'.
pad2 :: Monoid a => [a] -> [a] -> ([a], [a])
pad2 xs ys = (f xs, f ys)
  where
    f      = pad maxLen mempty
    maxLen = max (length xs) (length ys)

-- | 'pad' @limit defaultValue xs@ adds @defaultValue@s onto the end of @xs@
-- such that its length is @limit@. If @limit@ is less than the list's length
-- then the list is trimmed.
pad :: Int -> a -> [a] -> [a]
pad limit defaultVal xs
    | len > limit = take limit xs
    | otherwise   = xs ++ replicate (limit - len) defaultVal
    where len = length xs

--------------------------------------------------------------------------------
