--------------------------------------------------------------------------------

module Codeforces.App.Watcher where

import Codeforces.App.Format
import Codeforces.App.Table
import Codeforces.Error
import Codeforces.Logging

import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.Extra (forM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import System.Console.ANSI
import System.IO

--------------------------------------------------------------------------------

-- | Values used for watching output.
data WatchState = WatchState
    { wsTable      :: Table
    , wsUpdateTime :: Maybe UTCTime
    }

initWatchState :: WatchState
initWatchState = WatchState [] Nothing

--------------------------------------------------------------------------------

-- | 'handleWatch' @shouldWatch m@ runs computation @m@ once if @shouldWatch@ is
-- false, otherwise 'watchTable' watches it.
--
-- In the latter case, it clears the terminal and sets up terminal behaviour for
-- watching data.
--
handleWatch :: Bool -> IO (Either CodeforcesError Table) -> IO ()
handleWatch False m =
    m >>= either (putStrLn . elErrorMsg . showE) (mapM_ T.putStrLn)
handleWatch True m = withDisabledStdin $ do
    resetScreen
    evalStateT (watchTable 2 m) initWatchState

-- | 'watchTable' @delaySecs m@ runs computation @m@ every @delaySecs@ amount of
-- seconds. The terminal output from @m@ is changed if the next run of @m@
-- yields a different result.
watchTable
    :: Int                                  -- ^ Delay, in seconds.
    -> IO (Either CodeforcesError Table)    -- ^ Fetches an updated table.
    -> StateT WatchState IO ()              -- ^ Data from previous iteration.
watchTable delaySecs m = do
    oldState <- get
    now      <- lift getCurrentTime

    output   <- lift m >>= \case
        Left e -> do
            -- Include error message but table data remains the same
            let oldTable   = wsTable oldState
                lastUpdate = wsUpdateTime oldState
            pure $ addInfo oldTable (Just e) now lastUpdate

        Right newTable -> do
            let currUpdate = Just now
            put $ WatchState newTable currUpdate
            pure $ addInfo newTable Nothing now currUpdate

    lift $ do
        currTermH <- fmap fst <$> getTerminalSize
        let output' = truncateTable output (subtract 1 <$> currTermH)

        setCursorPosition 0 0
        forM_ output' $ \r -> clearLine >> T.putStrLn r

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

    errorMsg  = colored Red . (<> ". ") . T.pack . elErrorMsg . showE <$> me
    updateMsg = T.concat <$> sequenceA
        [Just "Last update: ", fmtDiffTime <$> (diffUTCTime now <$> lastUpdate)]

-- | 'truncateTable' @table x@ returns the table with the first @x@ rows, or the
-- original table if @x@ is @Nothing@.
truncateTable :: Table -> Maybe Int -> Table
truncateTable = maybe <*> flip take

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
