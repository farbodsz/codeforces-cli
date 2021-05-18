--------------------------------------------------------------------------------

-- | Implementation of the Open Codeforces Rating System described in 
-- <https://codeforces.com/blog/entry/20762 Mike Mirzayanov's blog post>.
--
module Codeforces.Virtual.RatingCalculator
    ( calculateContestResults
    ) where

import Codeforces.Party hiding (Contestant)
import Codeforces.Standings
import Codeforces.Types
import Codeforces.Virtual.Types

import Control.Monad
import Control.Monad.Trans.State

import Data.Functor ((<&>))
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord

--------------------------------------------------------------------------------

-- | 'calculateNewRatingChanges' @previousRatings updatedRankings@ computes the
-- contest results.
calculateContestResults :: M.Map Handle Int -> [RanklistRow] -> ContestResults
calculateContestResults hs rrs = ContestResults sortedCs deltas seeds
  where
    sortedCs        = reassignRanks $ mkContestants hs rrs
    (deltas, seeds) = process sortedCs

-- | Constructs a list of contestants from the previous ratings and rankings.
mkContestants :: M.Map Handle Int -> [RanklistRow] -> [Contestant]
mkContestants prevRatings = map
    (\RanklistRow {..} -> Contestant
        { contestantParty  = rrParty
        , contestantRank   = rrRank
        , contestantPoints = rrPoints
        , contestantRating = getPartyRating rrParty
        }
    )
  where
    getPartyRating = computePartyRating . map lookupRating . partyMembers
    lookupRating m = M.findWithDefault initRating (memberHandle m) prevRatings

--------------------------------------------------------------------------------

-- | Initial rating of a member, if they do not already have a rating.
initRating :: Int
initRating = 0

-- | Calculates the overall rating for a party using the ratings of its team
-- members.
--
-- >>> computePartyRating [1400]
-- 1400
--
-- >>> computePartyRating [1400, 1500, 1600]
-- 1749
--
computePartyRating :: [Int] -> Int
computePartyRating ratings = go 20 100 4000
  where
    go :: Int -> Float -> Float -> Int
    go 0 l r = round $ (l + r) / 2
    go i l r
        | computed > mid = go (i - 1) mid r
        | otherwise      = go (i - 1) l mid
      where
        mid = (l + r) / 2
        rWinsProbability =
            product $ map (getEloWinProbability mid . fromIntegral) ratings
        computed = logBase 10 (1 / rWinsProbability - 1) * 400 + mid

--------------------------------------------------------------------------------

-- | Ratings mapped to seed (expected ranking)
type SeedCache = M.Map Int Seed

-- | Computes each party's rating delta and each rating's seed, given a list of
-- contestants.
process :: [Contestant] -> (M.Map Party Delta, SeedCache)
process [] = (M.empty, M.empty)
process cs = flip runState (precomputeSeeds cs) $ do
    ds <- calculateDeltas cs
    pure . adjustTopDeltas cs . adjustAllDeltas $ ds

-- | Computes the seed of each contestant.
precomputeSeeds :: [Contestant] -> SeedCache
precomputeSeeds cs =
    M.fromList $ map (\c -> (contestantRating c, calculateSeedOf c cs)) cs

-- | Adjusts rating deltas to ensure the total sum of deltas is not more than
-- zero. If it is, the extra amount is distributed between all contestants.
adjustAllDeltas :: M.Map Party Delta -> M.Map Party Delta
adjustAllDeltas ds = M.map (+ inc) ds
    where inc = (negate (sum (M.elems ds)) `div` M.size ds) - 1

-- | Adjusts rating deltas to prevent ratings of top competitors becoming
-- inflated.
--
-- /Before/ the round, we choose a group of most highly rated competitors and
-- decide that their /total/ rating shouldn't change. The size of this group is
-- determined by the heuristic:
--
-- \[
-- s = \min(n, 4 \sqrt{n})
-- \]
--
-- The sum of deltas over this group is adjusted to make it 0:
--
-- \[
-- r_i = r_i - \frac{\sum^s d_i}{s}
-- \]
--
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

-- | Computes the rating delta for each party in this contest.
--
-- The input list of contestants must be correctly ordered with 'reassignRanks'
-- prior to using this function.
--
calculateDeltas :: [Contestant] -> State SeedCache (M.Map Party Delta)
calculateDeltas cs = do
    deltas <- forM cs $ \c -> calculateDelta c cs <&> (contestantParty c, )
    pure $ M.fromList deltas

-- | Sorts and recomputes the rank of each contestant.
--
-- In this assignment, contestants with the same points have the same rank.
-- Repeated ranks are disregarded when assigning ranks to contestants that
-- score lower than them. For example:
--
-- @
-- | Party | Points | Rank |
-- | ----- | ------ | ---- |
-- | A     | 2302.0 | 41   |
-- | B     | 2302.0 | 41   |
-- | C     | 2302.0 | 41   |
-- | D     | 2256.0 | 44   |   <- rank is not 42, but 44
-- | ...   | ...    | ...  |
-- @
--
-- Reassigning ranks is required because the input list of contestants is not
-- guaranteed to have correct ranks or be in the correct order following the
-- inclusion of the virtual user. E.g. the input list may resemble:
--
-- @
-- | Party | Points | Rank |
-- | ----- | ------ | ---- |
-- | ...   | ...    | ...  |
-- | A     | 2302.0 | 41   |
-- | VU*   | 2300.0 | 42   |
-- | B     | 2266.0 | 42   |   <- ranks from here on are incorrect
-- | C     | 2256.0 | 43   |
-- | ...   | ...    | ...  |
-- @
--
-- *VU = virtual user
--
reassignRanks :: [Contestant] -> [Contestant]
reassignRanks = go 1 1 . sortByPointsDesc
  where
    go _ _    []             = []
    go _ rank [c           ] = [withRank rank c]
    go i rank (c1 : c2 : cs) = withRank rank c1 : go (i + 1) nextRank (c2 : cs)
      where
        nextRank
            | contestantPoints c2 < contestantPoints c1 = i + 1
            | otherwise = rank

    withRank r c = c { contestantRank = r }

-- | 'calculateDelta' @c cs@ computes the rating delta for contestant @c@ using
-- a seed computed from all other contestants @cs@.
--
-- The rating change for a participant is the between the rating they require
-- (according to their seed) and their current rating:
--
-- \[
-- d_i = \frac{R - r_i}{2}
-- \]
--
calculateDelta :: Contestant -> [Contestant] -> State SeedCache Delta
calculateDelta c cs = do
    mid        <- midRank c cs
    needRating <- calculateNeedRating cs mid

    pure $ (needRating - contestantRating c) `div` 2

-- | The geometric mean of a contestant's seed (expected ranking) and actual
-- ranking.
--
-- This ranking is between the expected and actual ranking.
--
midRank :: Contestant -> [Contestant] -> State SeedCache Seed
midRank c cs = do
    seed <- getSeedOf c cs
    pure $ sqrt $ fromIntegral (contestantRank c) * seed

-- | Given a list of contestants and this contestant's 'midRank', calculates
-- the rating a contestant should have to achieve their expected ranking, using
-- binary search.
--
-- In other words, a rating:
--
-- \[
-- R : seed_i = m_i
-- \]
--
calculateNeedRating :: [Contestant] -> Float -> State SeedCache Int
calculateNeedRating cs rank = go 1 8000
  where
    go l r
        | r - l <= 1 = pure l
        | otherwise = do
            let mid = (l + r) `div` 2
            seed <- getSeed mid cs

            if seed < rank then go l mid else go mid r

--------------------------------------------------------------------------------
-- Seed calculations and lookups

-- | Looks up the seed for a given rating from the cache. If not found, computes
-- it and updates the cache.
getSeed :: Int -> [Contestant] -> State SeedCache Seed
getSeed rating cs = do
    cache <- get

    case M.lookup rating cache of
        Nothing -> do
            let seed = calculateSeed rating cs
            modify $ M.insert rating seed
            pure seed

        (Just seed) -> pure seed

-- | Like 'getSeed' but takes a contestant and list of /all/ contestants.
getSeedOf :: Contestant -> [Contestant] -> State SeedCache Seed
getSeedOf x ys = getSeed (contestantRating x) (filter (/= x) ys)

-- | Calculates the seed of a contestant with the given rating, using the
-- supplied list of all /other/ contestants.
--
-- \[
-- seed_i = \sum_{j=1, j \ne i}^{n} P_{j,i} + 1
-- \]
--
-- 1 is added to account for 1-based rankings.
--
-- The general idea is to increase the contestant's rating if their actual
-- ranking is better than their seed, and decrease if worse.
--
calculateSeed :: Int -> [Contestant] -> Seed
calculateSeed rating others =
    1 + sum [ getEloWinProbability' (contestantRating x) rating | x <- others ]

-- | Like 'calculateSeed' but takes a contestant and list of /all/ contestants.
calculateSeedOf :: Contestant -> [Contestant] -> Seed
calculateSeedOf x ys = calculateSeed (contestantRating x) (filter (/= x) ys)

-- | Computes the Elo win probability given two ratings.
--
-- This is the probability that the @x@th participant has a better result that
-- the @y@th participant, given by:
--
-- \[
-- P_{i,j} = \frac{1}{1 + 10^\frac{r_j - r_i}{400}}
-- \]
--
-- E.g. if the difference between ratings is 200 then the stronger participant
-- will win with probability ~0.75. If the difference is 400 then the stronger
-- participant will win with probability ~0.9.
--
-- __Note:__ reversing the order of rating arguments reverses the result.
--
-- >>> getEloWinProbability 1400 1200
-- 0.7597469
--
-- >>> getEloWinProbability 1200 1400
-- 0.24025308
--
getEloWinProbability :: Float -> Float -> Float
getEloWinProbability x y = 1 / (1 + 10 ** ((y - x) / 400))

-- | Like 'getEloWinProbability' but takes 'Int's instead of 'Float's. 
getEloWinProbability' :: Int -> Int -> Float
getEloWinProbability' x = getEloWinProbability (fromIntegral x) . fromIntegral

--------------------------------------------------------------------------------
-- Utility functions

sortByPointsDesc :: [Contestant] -> [Contestant]
sortByPointsDesc = sortOn (Down . contestantPoints)

sortByRatingDesc :: [Contestant] -> [Contestant]
sortByRatingDesc = sortOn (Down . contestantRating)

--------------------------------------------------------------------------------
