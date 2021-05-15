--------------------------------------------------------------------------------

module Codeforces
    ( module Codeforces.Contest
    , module Codeforces.Party
    , module Codeforces.Problem
    , module Codeforces.Rank
    , module Codeforces.RatingChange
    , module Codeforces.Standings
    , module Codeforces.Submission
    , module Codeforces.User
    , ResponseError(..)

    -- * Contests
    , getContests
    , getContestStandings
    , getContestStandingsWithUsers
    , StandingsParams(..)

    -- * Problems
    , getAllProblemData
    , getProblems

    -- * Ratings and ranks
    , getContestRatingChanges
    , getUserRatingHistory

    -- * Problem submissions
    , getContestSubmissions
    , getUserStatus

    -- * User details
    , getUser
    , getUsers
    , getFriends

    -- * Virtual rating calculation
    , calculateVirtualDelta
    , Delta
    , Seed
    , VirtualResult(..)

    -- * Configuration options
    , UserConfig(..)
    ) where

import Codeforces.Common
import Codeforces.Config
import Codeforces.Contest
import Codeforces.Party
import Codeforces.Problem
import Codeforces.Rank
import Codeforces.RatingChange
import Codeforces.Standings
import Codeforces.Submission
import Codeforces.User
import Codeforces.Virtual

import Control.Monad.Trans.Except

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------

getContests :: Bool -> IO (Either ResponseError [Contest])
getContests isGym = getData "/contest.list" [("gym", argBool isGym)]

-- | Contains query parameters for retrieving contest standings.
data StandingsParams = StandingsParams
    {
    -- | ID of the contest
      paramContestId  :: Int
    -- | The starting index of the ranklist (1-based)
    , paramFrom       :: Maybe Int
    -- | The number of standing rows to return
    , paramRowCount   :: Maybe Int
    -- | If specified, only standings of this room are returned
    , paramRoom       :: Maybe Int
    -- | If true, all participations are included. Otherwise only 'Contestant'
    -- participations are included.
    , paramUnofficial :: Bool
    -- | If specified, the standings includes only these users.
    , paramHandles    :: Maybe [Handle]
    }
    deriving Show

-- | 'getContestStandings' @standingsParams@ returns information about the
-- contest and a part of the standings list.
getContestStandings :: StandingsParams -> IO (Either ResponseError Standings)
getContestStandings StandingsParams {..} = getData
    "/contest.standings"
    [ ("contestId"     , argInt paramContestId)
    , ("from"          , argInt =<< paramFrom)
    , ("count"         , argInt =<< paramRowCount)
    , ("room"          , argInt =<< paramRoom)
    , ("showUnofficial", argBool paramUnofficial)
    , ("handles"       , T.encodeUtf8 . T.intercalate ";" <$> paramHandles)
    ]

-- | Like 'getContestStandings' but returns the standings and a map of users in
-- the standings list.
getContestStandingsWithUsers
    :: StandingsParams
    -> IO (Either ResponseError (Standings, M.Map Handle User))
getContestStandingsWithUsers q = runExceptT $ do
    ss <- ExceptT $ getContestStandings q
    let rl      = standingsRanklist ss
    let handles = concatMap (map memberHandle . partyMembers . rrParty) rl

    us <- ExceptT $ getUsers handles
    let uMap = M.fromList $ zip handles us

    pure (ss, uMap)

-- | 'getContestSubmissions' @contestId handle@ returns the submissions made by
-- the user in the contest given by @contestId@
getContestSubmissions
    :: Int -> Handle -> IO (Either ResponseError [Submission])
getContestSubmissions cId h = getData
    "/contest.status"
    [("contestId", argInt cId), ("handle", argHandle h)]

--------------------------------------------------------------------------------

-- | 'getAllProblemData' @tags@ returns a 'ProblemsResponse' filtered by the
-- @tags@, if supplied.
getAllProblemData :: [ProblemTag] -> IO (Either ResponseError ProblemsResponse)
getAllProblemData ts = getData "/problemset.problems" [("tags", argTags ts)]

-- | 'getProblems' @tags@ returns a list of 'Problem's containing the @tags@, if
-- provided.
getProblems :: [ProblemTag] -> IO (Either ResponseError [Problem])
getProblems ts = fmap prProblems <$> getAllProblemData ts

--------------------------------------------------------------------------------

-- | 'getContestRatingChanges' @contestId@ returns a  list of 'RatingChange's
-- for the contest.
getContestRatingChanges :: Int -> IO (Either ResponseError [RatingChange])
getContestRatingChanges cId =
    getData "/contest.ratingChanges" [("contestId", argInt cId)]

-- | 'getUserRatingHistory' @handle@ returns a list of 'RatingChange's for the
-- requested user
getUserRatingHistory :: Handle -> IO (Either ResponseError [RatingChange])
getUserRatingHistory h = getData "/user.rating" [("handle", argHandle h)]

--------------------------------------------------------------------------------

-- | 'getUser' @handle@ returns the 'User' with the given @handle@
getUser :: Handle -> IO (Either ResponseError User)
getUser h = fmap head <$> getUsers [h]

-- | 'getUsers' @handles@ returns a list of 'User's with the given @handles@
getUsers :: [Handle] -> IO (Either ResponseError [User])
getUsers [] = pure $ Right []
getUsers hs = getData "/user.info" [("handles", argHandles hs)]

-- 'getFriends' @config@ returns the handles of the friends of the currently
-- authenticated user.
getFriends :: UserConfig -> IO (Either ResponseError [Handle])
getFriends cfg = getAuthorizedData cfg "/user.friends" []

-- | 'getUserStatus' @handle from count@ returns the @count@ most recent
-- submissions by the user, starting from the @from@-th one.
getUserStatus :: Handle -> Int -> Int -> IO (Either ResponseError [Submission])
getUserStatus h f n = getData
    "/user.status"
    [("handle", argHandle h), ("from", argInt f), ("count", argInt n)]

--------------------------------------------------------------------------------

-- | 'calculateVirtualDelta' @contestId handle points penalty@ computes the
-- rating change the user would gain had they competed in the contest live.
--
-- Returns the results of the virtual user, if available.
--
calculateVirtualDelta
    :: Int
    -> Handle
    -> Points
    -> Int
    -> IO (Either ResponseError (Maybe VirtualResult))
calculateVirtualDelta cId handle points penalty = runExceptT $ do
    rcs       <- ExceptT $ getContestRatingChanges cId

    standings <- ExceptT $ getContestStandings $ StandingsParams
        { paramContestId  = cId
        , paramFrom       = Nothing
        , paramRowCount   = Nothing
        , paramRoom       = Nothing
        , paramUnofficial = False
        , paramHandles    = Nothing
        }

    user <- ExceptT $ getUser handle
    let vUser = VirtualUser
            { vuPoints     = points
            , vuPenalty    = penalty
            , vuCurrRating = userRating user
            }
        delta =
            calculateVirtualRatingChange vUser rcs (standingsRanklist standings)

    pure $ case delta of
        Nothing  -> Nothing
        (Just d) -> Just $ VirtualResult user d

--------------------------------------------------------------------------------

argBool :: Bool -> Maybe BC.ByteString
argBool = Just . BC.pack . show

argText :: T.Text -> Maybe BC.ByteString
argText = Just . T.encodeUtf8

argTexts :: [T.Text] -> Maybe BC.ByteString
argTexts xs
    | null xs   = Nothing
    | otherwise = (Just . T.encodeUtf8 . T.intercalate ";") xs

argHandle :: Handle -> Maybe BC.ByteString
argHandle = argText

argHandles :: [Handle] -> Maybe BC.ByteString
argHandles = argTexts

argInt :: Int -> Maybe BC.ByteString
argInt = Just . BC.pack . show

argTags :: [ProblemTag] -> Maybe BC.ByteString
argTags = argTexts

--------------------------------------------------------------------------------
