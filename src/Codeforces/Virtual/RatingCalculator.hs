--------------------------------------------------------------------------------

module Codeforces.Virtual.RatingCalculator
    ( calculateNewRatingChanges
    ) where

import Codeforces.Party hiding (Contestant)
import Codeforces.Standings
import Codeforces.User (Handle)
import Codeforces.Virtual.Types

import Control.Monad
import Control.Monad.Trans.State

import Data.Functor ((<&>))
import Data.List
import qualified Data.Map as M
import Data.Maybe

--------------------------------------------------------------------------------

-- | 'calculateNewRatingChanges' @previousRatings updatedRankings@ computes a
-- map of rating deltas for each user, and seeds for each contestant rating.
calculateNewRatingChanges
    :: M.Map Handle Int -> [RanklistRow] -> (M.Map Party Delta, M.Map Int Seed)
calculateNewRatingChanges hs rrs = process $ mkContestants hs rrs

-- | Constructs a list of contestants from the previous ratings and rankings.
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
-- See 'calculateDelta' for details.
--
calculateDeltas :: [Contestant] -> State SeedCache (M.Map Party Delta)
calculateDeltas cs = do
    let sorted = reassignRanks cs
    deltas <- forM sorted $ \c -> calculateDelta c cs <&> (contestantParty c, )
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

-- | r - l <= 1 = l
-- | r - l > 1 && calculateSeed mid cs < rank = go l mid
-- | otherwise  = go mid r

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
    1 + sum [ getEloWinProbability (contestantRating x) rating | x <- others ]

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
getEloWinProbability :: Int -> Int -> Float
getEloWinProbability x y = 1 / (1 + 10 ** (diff / 400))
    where diff = fromIntegral (y - x)

--------------------------------------------------------------------------------
-- Utility functions

sortByDesc :: (a -> Int) -> [a] -> [a]
sortByDesc f = sortOn (negate . f)

sortByPointsDesc :: [Contestant] -> [Contestant]
sortByPointsDesc = sortOn (negate . contestantPoints)

sortByRatingDesc :: [Contestant] -> [Contestant]
sortByRatingDesc = sortByDesc contestantRating

--------------------------------------------------------------------------------
