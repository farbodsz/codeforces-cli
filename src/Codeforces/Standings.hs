--------------------------------------------------------------------------------

module Codeforces.Standings where

import Codeforces.Common
import Codeforces.Contest (Contest)
import Codeforces.Party (Party)
import Codeforces.Problem (Points, Problem)

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.Time

--------------------------------------------------------------------------------

data ResultType
    -- | Means a party's points can decrease, e.g. if their solution fails
    -- during a system test.
    = ResultPreliminary
    -- | Means a party can only increase points for this problem by submitting
    -- better solutions.
    | ResultFinal

instance FromJSON ResultType where
    parseJSON = withText "ResultType" $ \case
        "PRELIMINARY" -> pure ResultPreliminary
        "FINAL"       -> pure ResultFinal
        _             -> fail "Invalid ResultType"

data ProblemResult = ProblemResult
    { prPoints               :: Points
    -- | Penalty (in ICPC meaning) of the party for this problem.
    , prPenalty              :: Maybe Int
    , prRejectedAttemptCount :: Int
    , prType                 :: ResultType
    -- | Number of seconds after the start of the contest before the submission,
    -- that brought maximal amount of points for this problem. 
    , prBestSubmissionTime   :: Maybe Int
    }

-- | True if no solution has been submitted for this problem in the contest.
prNotAttempted :: ProblemResult -> Bool
prNotAttempted ProblemResult {..} =
    prPoints == 0 && prRejectedAttemptCount == 0

instance FromJSON ProblemResult where
    parseJSON = withObject "ProblemResult" $ \v ->
        ProblemResult
            <$> (v .: "points")
            <*> (v .:? "penalty")
            <*> (v .: "rejectedAttemptCount")
            <*> (v .: "type")
            <*> (v .:? "bestSubmissionTime")

data RanklistRow = RanklistRow
    { rrParty                 :: Party
    , rrRank                  :: Int
    , rrPoints                :: Points
    , rrPenalty               :: Int
    , rrSuccessfulHackCount   :: Int
    , rrUnsuccessfulHackCount :: Int
    , rrProblemResults        :: [ProblemResult]
    -- | Time from the start of the contest to the last submission that added
    -- some points to the total score of the party. For IOI contests only.
    , rrLastSubmissionTime    :: Maybe DiffTime
    }

instance FromJSON RanklistRow where
    parseJSON = withObject "RanklistRow" $ \v ->
        RanklistRow
            <$> (v .: "party")
            <*> (v .: "rank")
            <*> (v .: "points")
            <*> (v .: "penalty")
            <*> (v .: "successfulHackCount")
            <*> (v .: "unsuccessfulHackCount")
            <*> (v .: "problemResults")
            <*> (fmap secondsToDiffTime <$> v .:? "lastSubmissionTimeSeconds")

--------------------------------------------------------------------------------

-- | The standings returned by the API consists of @Contest@ details, the list
-- of @Problems@ and the requested portion of the standings list (a list of
-- @RanklistRow@s).
data Standings = Standings
    { standingsContest  :: Contest
    , standingsProblems :: [Problem]
    , standingsRanklist :: [RanklistRow]
    }

instance FromJSON Standings where
    parseJSON =
        withObject "Standings"
            $ \v ->
                  Standings
                      <$> (v .: "contest")
                      <*> (v .: "problems")
                      <*> (v .: "rows")

-- | `getContestStandings` @contestId from count@ returns information about the
-- contest and a part of the standings list.
getContestStandings
    :: Int            -- ^ ID of the contest
    -> Int            -- ^ the starting index of the ranklist (1-based)
    -> Int            -- ^ number of standing rows to return
    -> Bool           -- ^ if false, only @Contestant@ participations returned
    -> IO (Either String Standings)
getContestStandings cId from count unofficial = getData
    "/contest.standings"
    [ ("contestId"     , asArg cId)
    , ("from"          , asArg from)
    , ("count"         , asArg count)
    , ("showUnofficial", asArg unofficial)
    ]
  where
    asArg :: Show a => a -> Maybe BC.ByteString
    asArg = Just . BC.pack . show

--------------------------------------------------------------------------------
