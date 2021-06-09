--------------------------------------------------------------------------------

module Watcher where

import Codeforces hiding (RankColor(..))

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.Extra (forM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import Format

import System.Console.ANSI
import System.IO

import Table

--------------------------------------------------------------------------------

-- | 'handleWatch' @shouldWatch m@ runs computation @m@ once if @shouldWatch@ is
-- false, otherwise 'watchTable' watches it.
--
-- In the latter case, it clears the terminal and sets up terminal behaviour for
-- watching data.
--
handleWatch :: Bool -> IO (Either CodeforcesError Table) -> IO ()
handleWatch False m = m >>= either (putStrLn . showE) (mapM_ T.putStrLn)
handleWatch True  m = withDisabledStdin $ do
    resetScreen
    evalStateT (watchTable 2 m) ([], Nothing)

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

    -- Truncate table height to terminal height if possible
    -- (getTerminalSize not supported on some Windows terminals)
    mTermSize <- lift getTerminalSize
    let newTable' = case mTermSize of
            Nothing     -> newTable
            Just (h, _) -> take (h - 1) newTable

    -- Print/store currently displayed table with the time it was retrieved
    put (newTable', updateTime)

    lift $ do
        setCursorPosition 0 0
        forM_ newTable' $ \r -> clearLine >> T.putStrLn r

        threadDelay (delaySecs * 1000000)

    watchTable delaySecs m

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

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

-- | Prevents any input into the terminal while running the IO computation.
withDisabledStdin :: IO a -> IO a
withDisabledStdin io = bracket
    (do
        prevBuff <- hGetBuffering stdin
        prevEcho <- hGetEcho stdin

        -- Disable terminal input
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False

        pure (prevBuff, prevEcho)
    )
    (\(prevBuff, prevEcho) ->
        -- Revert stdin buffering and echo
        hSetBuffering stdin prevBuff >> hSetEcho stdin prevEcho
    )
    (const io)

--------------------------------------------------------------------------------
