--------------------------------------------------------------------------------

-- | Virtual contest/rating command.
module Commands.VirtualCmd
    ( virtualRating
    ) where

import Codeforces

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Error
import Format

--------------------------------------------------------------------------------

data VirtualError = VirtualNoResult

instance CodeforcesError VirtualError where
    showE VirtualNoResult =
        "An unexpected error occurred.\n"
            ++ "Your rating change could not be calculated."

--------------------------------------------------------------------------------

virtualRating :: ContestId -> Handle -> Points -> Int -> IO ()
virtualRating cId h pts pen = handleE $ runExceptT $ do
    (u, mRes) <- ExceptT $ calculateVirtualResult cId h pts pen

    lift $ handleE $ runExceptT $ case mRes of
        Nothing  -> throwE VirtualNoResult
        Just res -> lift $ printVirtualRes u res

printVirtualRes :: User -> VirtualResult -> IO ()
printVirtualRes u VirtualResult {..} = do
    printRankings virtualSeed virtualRank

    putStrLn ""
    putStrLn "Rating change:"

    let currRating = userRating u
    let currRank   = getRank currRating
    let newRating  = currRating + virtualDelta
    let newRank    = getRank newRating

    putStrLn $ concat ["  (", show currRating, " -> ", show newRating, ")"]
    putStrLn ""

    T.putStrLn $ T.concat ["  ", indent, diffColored virtualDelta]
    putStrLn ""

    let handle = unHandle (userHandle u)
        desc   = if currRank == newRank
            then T.concat
                [ "Would remain "
                , rankName currRank
                , " "
                , rankColored (rankColor currRank) handle
                ]
            else T.concat
                [ "Would become "
                , rankName newRank
                , ": "
                , rankColored (rankColor currRank) handle
                , " -> "
                , rankColored (rankColor newRank) handle
                ]
    T.putStrLn $ T.concat ["  ", desc, "\n"]

printRankings :: Seed -> Int -> IO ()
printRankings seed rank = do
    putStrLn ""
    putStrLn $ "Expected ranking: " ++ show (round seed :: Int)
    putStrLn $ "Actual ranking:   " ++ show rank

-------------------------------------------------------------------------------
