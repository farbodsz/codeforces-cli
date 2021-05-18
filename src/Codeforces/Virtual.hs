--------------------------------------------------------------------------------

module Codeforces.Virtual
    ( VirtualUser(..)
    , VirtualResult(..)
    , Delta
    , Seed
    , calculateResult
    ) where

import Codeforces.RatingChange
import Codeforces.Standings
import Codeforces.User
import Codeforces.Virtual.RatingCalculator
import Codeforces.Virtual.Types

import Control.Applicative

import Data.Bifunctor (bimap)
import Data.Bitraversable (bisequence)
import qualified Data.Map as M

--------------------------------------------------------------------------------

-- | Computes the rating change the user would have undergone had they
-- participated in this contest live with their current rating, with their
-- expected ranking for the contest (seed).
calculateResult
    :: VirtualUser          -- ^ Details about the virtual participation.
    -> [RatingChange]       -- ^ Rating changes for this contest
    -> [RanklistRow]        -- ^ Standings for this contest.
    -> Maybe (Delta, Seed)  -- ^ User's delta and seed for the contest
calculateResult vu@VirtualUser {..} rcs rrs = bisequence
    $ bimap (M.lookup virtualParty) (M.lookup vuRating) result
  where
    result          = calculateNewRatingChanges updatedRatings updatedRankings
    updatedRatings  = M.insert virtualHandle vuRating (previousRatings rcs)
    updatedRankings = virtualRankings vu rrs

-- | 'previousRatings' @ratingChanges@ returns a map of each user's handle to
-- their rating before the contest.
previousRatings :: [RatingChange] -> M.Map Handle Int
previousRatings = M.fromList . map (liftA2 (,) rcHandle rcOldRating)

--------------------------------------------------------------------------------

-- | Builds an updated list of 'RanklistRow's for this contest by finding the
-- virtual user's rank and including them in the list.
virtualRankings :: VirtualUser -> [RanklistRow] -> [RanklistRow]
virtualRankings vu rrs = go rrs 1
  where
    go [] rank = [mkVirtualRow rank vu]
    go (x : xs) _
        | shouldInsert vu x = mkVirtualRow (rrRank x) vu : x : xs
        | otherwise         = x : go xs (rrRank x + 1)

    -- | Whether to insert the virtual user's row before the given row.
    shouldInsert VirtualUser {..} RanklistRow {..} =
        (vuPoints > rrPoints)
            || (vuPoints == rrPoints && vuPenalty <= rrPenalty)

-- | Constructs the virtual user's 'RanklistRow', using the virtual user's rank,
-- and their contest results.
mkVirtualRow :: Int -> VirtualUser -> RanklistRow
mkVirtualRow virtualRank VirtualUser {..} = RanklistRow
    { rrParty                 = virtualParty
    , rrRank                  = virtualRank
    , rrPoints                = vuPoints
    , rrPenalty               = vuPenalty
    , rrSuccessfulHackCount   = 0
    , rrUnsuccessfulHackCount = 0
    , rrProblemResults        = []
    , rrLastSubmissionTime    = Nothing
    }

--------------------------------------------------------------------------------
