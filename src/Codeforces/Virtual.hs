--------------------------------------------------------------------------------

module Codeforces.Virtual
    ( VirtualUser(..)
    , Delta
    , calculateVirtualRatingChange
    ) where

import Codeforces.Party hiding (Contestant)
import Codeforces.Problem (Points)
import Codeforces.RatingChange
import Codeforces.Standings
import Codeforces.User

import Control.Applicative

import Data.List
import qualified Data.Map as M
import Data.Maybe

--------------------------------------------------------------------------------

virtualHandle :: Handle
virtualHandle = "VIRTUAL_USER"

-- | A 'Party' representing the user's virtual participation.
virtualParty :: Party
virtualParty = Party
    { partyContestId       = Nothing
    , partyMembers         = [Member virtualHandle]
    , partyParticipantType = Virtual
    , partyTeamId          = Nothing
    , partyTeamName        = Nothing
    , partyIsGhost         = False
    , partyRoom            = Nothing
    , partyStartTime       = Nothing
    }

-- | Represents the virtual participation of the user in this contest.
data VirtualUser = VirtualUser
    { vuHandle     :: Handle  -- ^ Identifies the user in the virtual contest.
    , vuPoints     :: Points  -- ^ Points scored in the virtual contest.
    , vuPenalty    :: Int     -- ^ User's penalty in the virtual contest.
    , vuCurrRating :: Int     -- ^ Current rating of the user.
    }

--------------------------------------------------------------------------------

-- | Computes the rating change the user would have undergone had they
-- participated in this contest live with their current rating.
calculateVirtualRatingChange
    :: VirtualUser      -- ^ Details about the virtual participation.
    -> [RatingChange]   -- ^ Rating changes for this contest
    -> [RanklistRow]    -- ^ Standings for this contest.
    -> Maybe Delta      -- ^ User's delta had they participated as contestant
calculateVirtualRatingChange vu@VirtualUser {..} rcs rrs = M.lookup
    virtualParty
    updatedDeltas
  where
    previousRatings = M.insert vuHandle vuCurrRating (mapOfPreviousRatings rcs)
    updatedRankings = virtualRankings vu rrs
    updatedDeltas   = calculateNewRatingChanges previousRatings updatedRankings

-- | 'mapOfPreviousRatings' @ratingChanges@ returns a map of each user's handle
-- to their rating before the contest.
mapOfPreviousRatings :: [RatingChange] -> M.Map Handle Int
mapOfPreviousRatings = M.fromList . map (liftA2 (,) rcHandle rcOldRating)

--------------------------------------------------------------------------------

-- | Builds an updated list of 'RanklistRow's for this contest by finding the
-- virtual user's rank and including them in the list.
virtualRankings :: VirtualUser -> [RanklistRow] -> [RanklistRow]
virtualRankings _  []       = []
virtualRankings vu (x : xs) = if shouldInsert vu (rrPoints x) (rrPenalty x)
    then mkVirtualRow (rrRank x) vu : x : xs
    else x : virtualRankings vu xs
  where
    -- | Whether to insert the virtual user at this point in the ranklist rows.
    shouldInsert VirtualUser {..} rowPoints rowPenalty
        | vuPoints == rowPoints = vuPenalty <= rowPenalty
        | otherwise             = vuPoints > rowPoints

-- | Constructs the virtual user's 'RanklistRow', using the virtual user's rank,
-- and their  contest results.
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
-- Types

-- | Difference in rating between a user's current rating, and their rating
-- following this contest.
type Delta = Int

-- | The seed is the expected ranking for each participant before the contest
-- begins.
--
-- A contestant's rating increases should they perform better than their seed,
-- and decreases should they perform worse.
--
type Seed = Float

-- | The participation of a user in a contest.
data Contestant = Contestant
    { contestantParty  :: Party
    , contestantRank   :: Int
    , contestantPoints :: Points
    , contestantRating :: Int
    }
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | 'calculateNewRatingChanges' @previousRatings updatedRankings@ computes a
-- map of rating deltas for each user that take into account the virtual user's
-- participation.
calculateNewRatingChanges
    :: M.Map Handle Int -> [RanklistRow] -> M.Map Party Delta
calculateNewRatingChanges hs rrs = process $ mkContestants hs rrs

-- | Constructs a list of contestants from the previous ratings and rankings.
-- Each contestant's rank and delta is 0.
mkContestants :: M.Map Handle Int -> [RanklistRow] -> [Contestant]
mkContestants prevRatings = map
    (\RanklistRow {..} -> Contestant
        { contestantParty  = rrParty
        , contestantRank   = rrRank
        , contestantPoints = rrPoints
        , contestantRating = findContestantRating rrParty
        }
    )
  where
    -- TODO: Calculate properly
    -- avgMemberRating = average . catMaybes . memberRatings
    -- average         = liftA2 div sum length
    -- memberRatings =
    --     map (flip M.lookup prevRatings. memberHandle) . partyMembers

    findContestantRating p = case partyMembers p of
        [m] -> fromMaybe 0 $ M.lookup (memberHandle m) prevRatings
        _   -> 0

--------------------------------------------------------------------------------

-- | Computes each party's rating delta with the necessary adjustments, given a
-- list of contestants.
process :: [Contestant] -> M.Map Party Delta
process [] = M.empty
process cs = adjustTopDeltas cs . adjustAllDeltas . calculateDeltas $ cs

-- | Adjusts rating deltas to ensure the total sum of deltas is not more than
-- zero. If it is, the extra amount is distributed between all contestants.
adjustAllDeltas :: M.Map Party Delta -> M.Map Party Delta
adjustAllDeltas ds = M.map (+ inc) ds
    where inc = (negate (sum (M.elems ds)) `div` M.size ds) - 1

-- | Adjusts rating deltas to ensure that the sum of the top deltas is zero.
adjustTopDeltas :: [Contestant] -> M.Map Party Delta -> M.Map Party Delta
adjustTopDeltas cs ds = M.map (+ inc) ds
  where
    inc          = min 0 $ max (-10) (negate sumTopDeltas `div` zeroSumCount)

    sumTopDeltas = sum $ mapMaybe
        (flip M.lookup ds . contestantParty)
        (take zeroSumCount $ sortByRatingDesc cs)

    zeroSumCount = min (M.size ds) topCount
    topCount     = 4 * (round' . sqrt . fromIntegral . M.size) ds
    round'       = round :: Double -> Int

validateDeltas :: M.Map Party Delta -> M.Map Party Delta
validateDeltas = id -- TODO

-- | Computes the rating delta for each party in this contest.
calculateDeltas :: [Contestant] -> M.Map Party Delta
calculateDeltas cs = M.fromList $ map
    (\c -> (contestantParty c, calculateDelta c cs))
    (reassignRanks cs)

-- | Sorts and recomputes the rank of each contestant.
-- Contestants with the same points have the same rank.
reassignRanks :: [Contestant] -> [Contestant]
reassignRanks = go 1 . sortByPointsDesc
  where
    go _    []  = []
    go rank [c] = [withRank rank c]
    go rank (c1 : c2 : cs) =
        let
            nextRank = if contestantPoints c2 < contestantPoints c1
                then rank + 1
                else rank
        in withRank rank c1 : go nextRank (c2 : cs)

    withRank r c = c { contestantRank = r }

--------------------------------------------------------------------------------
-- Calculating deltas, seeds and probabilities

-- | 'calculateDelta' @c cs@ computes the rating delta for contestant @c@ using
-- a seed computed from all other contestants @cs@.
calculateDelta :: Contestant -> [Contestant] -> Delta
calculateDelta c cs = (needRating - contestantRating c) `div` 2
  where
    midRank    = sqrt $ fromIntegral (contestantRank c) * calculateSeedOf c cs
    needRating = calculateNeedRating cs midRank

-- | Calculates the seed of a contestant with the given rating, using the
-- supplied list of all /other/ contestants.
--
-- \[
-- seed_i = \sum_{j=1, j \ne i}^{n} P_{j,i} + 1
-- \]
--
calculateSeed :: Int -> [Contestant] -> Seed
calculateSeed rating others =
    1 + sum [ getEloWinProbability rating (contestantRating x) | x <- others ]

-- | Helper function like 'calculateSeed' but takes a contestant and a list of
-- all contestants.
calculateSeedOf :: Contestant -> [Contestant] -> Seed
calculateSeedOf x ys = calculateSeed (contestantRating x) (filter (/= x) ys)

-- | Computes the Elo win probability given two ratings.
--
-- This is the probability that the @x@th participant has a better result that
-- the @y@th participant, given by:
--
-- \[
-- P_{i,j} = \frac{1}{1 + 10 \frac{r_j - r_i}{400}}
-- \]
--
getEloWinProbability :: Int -> Int -> Float
getEloWinProbability x y = 1 / (1 + 10 ** (diff / 400))
    where diff = fromIntegral (y - x)

-- | Calculates the rating needed by a contestant with the given rank using
-- binary search.
calculateNeedRating :: [Contestant] -> Float -> Int
calculateNeedRating cs rank = go 1 8000
  where
    go l r
        | r - l <= 1 = l
        | r - l > 1 && calculateSeed mid cs < rank = go l mid
        | otherwise  = go mid r
        where mid = (l + r) `div` 2

--------------------------------------------------------------------------------
-- Utility functions

sortByDesc :: (a -> Int) -> [a] -> [a]
sortByDesc f = sortOn (negate . f)

sortByPointsDesc :: [Contestant] -> [Contestant]
sortByPointsDesc = sortOn (negate . contestantPoints)

sortByRatingDesc :: [Contestant] -> [Contestant]
sortByRatingDesc = sortByDesc contestantRating

--------------------------------------------------------------------------------
