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

    -- * Problems
    , getAllProblemData
    , getProblems

    -- * Ratings and ranks
    , getUserRatingHistory

    -- * Problem submissions
    , getContestSubmissions
    , getUserStatus

    -- * User details
    , getUser
    , getUsers
    , getFriends

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

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------

getContests :: Bool -> IO (Either ResponseError [Contest])
getContests isGym = getData "/contest.list" [("gym", argBool isGym)]

-- | 'getContestStandings' @contestId from count@ returns information about the
-- contest and a part of the standings list.
getContestStandings
    :: Int            -- ^ ID of the contest
    -> Int            -- ^ the starting index of the ranklist (1-based)
    -> Int            -- ^ number of standing rows to return
    -> Maybe Int      -- ^ if specified, only standings of the room are returned
    -> Bool           -- ^ if false, only @Contestant@ participations returned
    -> Maybe [Handle] -- ^ if specified, the list of handles to show
    -> IO (Either ResponseError Standings)
getContestStandings cId from count mroom unofficial hs = getData
    "/contest.standings"
    [ ("contestId"     , argInt cId)
    , ("from"          , argInt from)
    , ("count"         , argInt count)
    , ("room"          , argInt =<< mroom)
    , ("showUnofficial", argBool unofficial)
    , ("handles"       , T.encodeUtf8 . T.intercalate ";" <$> hs)
    ]

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

argBool :: Bool -> Maybe BC.ByteString
argBool = Just . BC.pack . show

argText :: T.Text -> Maybe BC.ByteString
argText = Just . T.encodeUtf8

argTexts :: [T.Text] -> Maybe BC.ByteString
argTexts = argText . T.intercalate ";"

argHandle :: Handle -> Maybe BC.ByteString
argHandle = argText

argHandles :: [Handle] -> Maybe BC.ByteString
argHandles = argTexts

argInt :: Int -> Maybe BC.ByteString
argInt = Just . BC.pack . show

argTags :: [ProblemTag] -> Maybe BC.ByteString
argTags ts = if null ts then Nothing else argTexts ts

--------------------------------------------------------------------------------
