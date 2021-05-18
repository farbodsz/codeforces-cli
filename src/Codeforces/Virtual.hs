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

import qualified Data.Map as M

--------------------------------------------------------------------------------

-- | Computes the results the user would have had, had they participated in this
-- contest live with their current rating.
calculateResult
    :: VirtualUser          -- ^ Details about the virtual participation.
    -> [RatingChange]       -- ^ Rating changes for this contest
    -> [RanklistRow]        -- ^ Standings for this contest.
    -> Maybe VirtualResult  -- ^ Contest results for this user
calculateResult vu rcs rrs =
    VirtualResult
        <$> (contestantRank <$> findContestant virtualParty crContestants)
        <*> M.lookup virtualParty crDeltas
        <*> M.lookup (vuRating vu) crSeeds
    where ContestResults {..} = computeResults vu rcs rrs

-- | Computes the complete updated results for a contest after including the
-- virtual user.
computeResults
    :: VirtualUser -> [RatingChange] -> [RanklistRow] -> ContestResults
computeResults vu@VirtualUser {..} rcs rrs = calculateContestResults
    updatedRatings
    updatedRankings
  where
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
