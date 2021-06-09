--------------------------------------------------------------------------------

module Watcher where

import Codeforces hiding (RankColor(..))

import Control.Concurrent
import Control.Monad (zipWithM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import Format

import System.Console.ANSI

import Table

--------------------------------------------------------------------------------

-- | 'handleWatch' @shouldWatch m@ runs computation @m@ once if @shouldWatch@ is
-- false, otherwise 'watchTable' watches it.
handleWatch :: Bool -> IO (Either CodeforcesError Table) -> IO ()
handleWatch False m = m >>= either (putStrLn . showE) (mapM_ T.putStrLn)
handleWatch True  m = clearScreen >> evalStateT (watchTable 5 m) ([], Nothing)

-- | 'watchTable' @delaySecs m@ runs computation @m@ every @delaySecs@ amount of
-- seconds. The terminal output from @m@ is changed if the next run of @m@
-- yields a different result.
watchTable
    :: Int                                  -- ^ Delay, in seconds.
    -> IO (Either CodeforcesError Table)    -- ^ Fetches an updated table.
    -> StateT (Table, Maybe UTCTime) IO ()  -- ^ Table and last update time.
watchTable delaySecs m = do
    (oldTable, lastUpdate) <- get
    now                    <- lift getCurrentTime

    let currUpdate = Just now

    -- Fetch updated table and add information rows
    (newTable, updateTime) <- lift m >>= \case
        Left  e -> pure (addInfo oldTable (Just e) now lastUpdate, lastUpdate)
        Right t -> pure (addInfo t Nothing now currUpdate, currUpdate)

    -- Print/store currently displayed table with the time it was retrieved
    put (newTable, updateTime)
    lift $ updateTableOutput oldTable newTable

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

-- | Takes a table with a potential error message and last successful update
-- time, and returns the table with extra rows for various messages.
--
-- When tables are displayed in watch mode, these extra rows are placed before
-- the table:
--  * The first displays a message for how to exit watch mode.
--  * The second row displays errors if there are any and the last update time.
--  * The third row is blank.
--
addInfo
    :: Table                  -- ^ Table to modify.
    -> Maybe CodeforcesError  -- ^ Error message, if exists.
    -> UTCTime                -- ^ Current system time.
    -> Maybe UTCTime          -- ^ Last successful update time, if any.
    -> Table
addInfo table me now lastUpdate =
    colored Cyan "Watching for updates. Press CTRL+c to exit."
        : statusMsg
        : ""
        : table
  where
    statusMsg = T.concat $ catMaybes [errorMsg, updateMsg]

    errorMsg  = colored Red . (<> " ") . T.pack . showE <$> me
    updateMsg = T.concat <$> sequenceA
        [Just "Last update: ", fmtDiffTime <$> (diffUTCTime now <$> lastUpdate)]

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
