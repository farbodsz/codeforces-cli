--------------------------------------------------------------------------------

-- | Problems command.
module Commands.ProblemsCmd
    ( problemList
    ) where

import Codeforces

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Data.Text.IO as T

import Error
import Format
import Options
import Table

--------------------------------------------------------------------------------

problemList :: ProblemOpts -> IO ()
problemList ProblemOpts {..} = handleE $ runExceptT $ do
    let ratingBounds = inRatingRange (optMinRating, optMaxRating)

    problems <- ExceptT $ fmap (filter ratingBounds) <$> getProblems []

    let headers = [("#", 6), ("Name", 40), ("Rating", 6)]
        rows    = map
            (\Problem {..} ->
                [ plainCell $ maybe "" showText problemContestId <> problemIndex
                , plainCell problemName
                , maybe blankCell ratingCell problemRating
                ]
            )
            problems

    lift $ mapM_ T.putStrLn $ makeTable headers rows

inRatingRange :: (Rating, Rating) -> Problem -> Bool
inRatingRange (minr, maxr) p = case problemRating p of
    Nothing -> False
    Just r  -> minr <= r && r <= maxr

--------------------------------------------------------------------------------
