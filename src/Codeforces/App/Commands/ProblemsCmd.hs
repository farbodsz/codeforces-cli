--------------------------------------------------------------------------------

-- | Problems command.
module Codeforces.App.Commands.ProblemsCmd
    ( problemList
    ) where

import           Codeforces.API
import           Codeforces.App.Format
import           Codeforces.App.Options
import           Codeforces.App.Table
import           Codeforces.Error

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as T

--------------------------------------------------------------------------------

problemList :: ProblemOpts -> IO ()
problemList ProblemOpts {..} = handleE $ runExceptT $ do
    let ratingBounds = inRatingRange (optMinRating, optMaxRating)

    problems <- handleAPI $ fmap (filter ratingBounds) <$> getProblems []

    let headers = [("#", 6), ("Name", 40), ("Rating", 6)]
        rows    = map
            (\Problem {..} ->
                [ plainCell $ fmtProblemIndex problemContestId problemIndex
                , plainCell problemName
                , maybe blankCell ratingCell problemRating
                ]
            )
            problems

    lift $ mapM_ T.putStrLn $ makeTable headers rows

fmtProblemIndex :: Maybe ContestId -> ProblemIndex -> Text
fmtProblemIndex cId pIdx = maybe "" (showText . unContestId) cId <> pIdx

inRatingRange :: (Rating, Rating) -> Problem -> Bool
inRatingRange (minr, maxr) p = case problemRating p of
    Nothing -> False
    Just r  -> minr <= r && r <= maxr

--------------------------------------------------------------------------------
