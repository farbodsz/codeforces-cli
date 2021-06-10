--------------------------------------------------------------------------------

module Codeforces.API
    ( module Codeforces.Types
    , ResponseError(..)
    , handleAPI

    -- * Contests
    , getContests
    , getContestStandings
    , getContestStandings'
    , StandingsParams(..)

    -- * Problems
    , getAllProblemData
    , getProblems
    , getProblemStats
    , getContestProblems

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
    , calculateVirtualResult
    , Delta
    , Seed
    , VirtualResult(..)

    -- * Configuration options
    , UserConfig(..)
    ) where

import Codeforces.Config
import Codeforces.Error
import Codeforces.Response
import Codeforces.Types
import Codeforces.Virtual

import Control.Arrow (left)
import Control.Monad.Trans.Except

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------

handleAPI :: IO (Either ResponseError a) -> ExceptT CodeforcesError IO a
handleAPI m = ExceptT $ left ResponseError <$> m

--------------------------------------------------------------------------------

-- | Query parameters for retrieving contest standings.
data StandingsParams = StandingsParams
    {
    -- | ID of the contest
      paramContestId  :: ContestId
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

--------------------------------------------------------------------------------

-- | 'getContests' @isGym@ returns a list of contests that may or may not be gym
-- contests.
getContests :: Bool -> IO (Either ResponseError [Contest])
getContests isGym = getData "/contest.list" [("gym", argBool isGym)]

-- | 'getContestStandings' @standingsParams@ returns information about the
-- contest and a part of the standings list.
getContestStandings :: StandingsParams -> IO (Either ResponseError Standings)
getContestStandings StandingsParams {..} = getData
    "/contest.standings"
    [ ("contestId"     , argContestId paramContestId)
    , ("from"          , argInt =<< paramFrom)
    , ("count"         , argInt =<< paramRowCount)
    , ("room"          , argInt =<< paramRoom)
    , ("showUnofficial", argBool paramUnofficial)
    , ("handles"       , argHandles =<< paramHandles)
    ]

-- | Like 'getContestStandings' but returns the standings and the
-- 'RatingChange's for each user participating.
getContestStandings'
    :: StandingsParams
    -> IO (Either ResponseError (Standings, M.Map Handle RatingChange))
getContestStandings' params = runExceptT $ do
    ss  <- ExceptT $ getContestStandings params
    rcs <- ExceptT $ getContestRatingChanges (paramContestId params)

    let rcsMap = M.fromList $ map (rcHandle >>= (,)) rcs

    pure (ss, rcsMap)

-- | 'getContestSubmissions' @contestId handle@ returns the submissions made by
-- the user in the contest given by @contestId@
getContestSubmissions
    :: ContestId -> Handle -> IO (Either ResponseError [Submission])
getContestSubmissions cId h = getData
    "/contest.status"
    [("contestId", argContestId cId), ("handle", argHandle h)]

--------------------------------------------------------------------------------

-- | 'getAllProblemData' @tags@ returns a 'ProblemsResponse' filtered by the
-- @tags@, if supplied.
getAllProblemData :: [ProblemTag] -> IO (Either ResponseError ProblemsResponse)
getAllProblemData ts = getData "/problemset.problems" [("tags", argTags ts)]

-- | 'getProblems' @tags@ returns a list of 'Problem's containing the @tags@, if
-- provided.
getProblems :: [ProblemTag] -> IO (Either ResponseError [Problem])
getProblems ts = fmap prProblems <$> getAllProblemData ts

-- | Like 'getProblems' but returns a list of 'ProblemStats'.
getProblemStats :: [ProblemTag] -> IO (Either ResponseError [ProblemStats])
getProblemStats ts = fmap prStats <$> getAllProblemData ts

-- | 'getContestProblems' @contestId@ returns the list of problems for the given
-- contest.
--
-- This should be used instead of filtering results from 'getProblems' for two
-- main reasons:
--
--     (1) 'problemContestId' can only refer to one contest, so problems
--         appearing in multiple contests may not be filtered correctly.
--     (2) 'getProblems' returns larger output potentially affecting performance
--
getContestProblems :: ContestId -> IO (Either ResponseError [Problem])
getContestProblems cId = fmap standingsProblems <$> getContestStandings
    StandingsParams
        { paramContestId  = cId
        , paramFrom       = Just 1
        , paramRowCount   = Just 1
        , paramRoom       = Nothing
        , paramUnofficial = False
        , paramHandles    = Nothing
        }

--------------------------------------------------------------------------------

-- | 'getContestRatingChanges' @contestId@ returns a list of 'RatingChange's
-- for the contest.
getContestRatingChanges
    :: ContestId -> IO (Either ResponseError [RatingChange])
getContestRatingChanges cId =
    getData "/contest.ratingChanges" [("contestId", argContestId cId)]

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

-- | 'calculateVirtualResult' @contestId handle points penalty@ computes the
-- rating change the user would gain had they competed in the contest live, and
-- their expected ranking for the contest.
--
calculateVirtualResult
    :: ContestId
    -> Handle
    -> Points
    -> Int
    -> IO (Either ResponseError (User, Maybe VirtualResult))
calculateVirtualResult cId handle points penalty = runExceptT $ do
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
            { vuPoints  = points
            , vuPenalty = penalty
            , vuRating  = userRating user
            }
        result = calculateResult vUser rcs (standingsRanklist standings)

    pure (user, result)

--------------------------------------------------------------------------------

argBool :: Bool -> Maybe BC.ByteString
argBool = Just . BC.pack . show

argText :: T.Text -> Maybe BC.ByteString
argText = Just . T.encodeUtf8

argTexts :: [T.Text] -> Maybe BC.ByteString
argTexts xs
    | null xs   = Nothing
    | otherwise = (Just . T.encodeUtf8 . T.intercalate ";") xs

argInt :: Int -> Maybe BC.ByteString
argInt = Just . BC.pack . show

argContestId :: ContestId -> Maybe BC.ByteString
argContestId = argInt . unContestId

argHandle :: Handle -> Maybe BC.ByteString
argHandle = argText . unHandle

argHandles :: [Handle] -> Maybe BC.ByteString
argHandles = argTexts . map unHandle

argTags :: [ProblemTag] -> Maybe BC.ByteString
argTags = argTexts

--------------------------------------------------------------------------------
