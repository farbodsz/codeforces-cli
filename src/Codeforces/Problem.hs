--------------------------------------------------------------------------------

module Codeforces.Problem
    ( ProblemIndex
    , Points
    , ProblemTag
    , ProblemType(..)
    , Problem(..)
    , ProblemStats(..)
    , getProblems
    ) where

import Codeforces.Common

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.List (intercalate)

--------------------------------------------------------------------------------

-- | A letter, or letter with digit(s) indicating the problem index in a
-- contest.
type ProblemIndex = String

type Points = Float

type ProblemTag = String

data ProblemType = Programming | Question
    deriving Show

instance FromJSON ProblemType where
    parseJSON = withText "ProblemType" $ \case
        "PROGRAMMING" -> pure Programming
        "QUESTION"    -> pure Question
        _             -> fail "Invalid ProblemType"

data Problem = Problem
    { problemContestId :: Maybe Int
    , problemSetName   :: Maybe String
    , problemIndex     :: ProblemIndex
    , problemName      :: String
    , problemType      :: ProblemType
    , problemPoints    :: Maybe Points
    , problemRating    :: Maybe Int
    , problemTags      :: [ProblemTag]
    }
    deriving Show

instance FromJSON Problem where
    parseJSON = withObject "Problem" $ \v ->
        Problem
            <$> (v .:? "contestId")
            <*> (v .:? "problemsetName")
            <*> (v .: "index")
            <*> (v .: "name")
            <*> (v .: "type")
            <*> (v .:? "points")
            <*> (v .:? "rating")
            <*> (v .: "tags")

data ProblemStats = ProblemStats
    { pStatContestId    :: Maybe Int
    , pStatProblemIndex :: ProblemIndex
    , pStatSolvedCount  :: Int
    }
    deriving Show

instance FromJSON ProblemStats where
    parseJSON = withObject "ProblemStats" $ \v ->
        ProblemStats
            <$> (v .:? "contestId")
            <*> (v .: "index")
            <*> (v .: "solvedCount")

--------------------------------------------------------------------------------

-- | Problem data returned by the API contains two lists: a list of problems 
-- followed by a list of corresponding problem statistics.
data ProblemsResponse = ProblemsResponse
    { prProblems :: [Problem]
    , prStats    :: [ProblemStats]
    }

instance FromJSON ProblemsResponse where
    parseJSON =
        withObject "ProblemsResponse"
            $ \v ->
                  ProblemsResponse
                      <$> (v .: "problems")
                      <*> (v .: "problemStatistics")

-- | `getAllProblemData` @tags@ returns a @ProblemResponse@ filtered by the
-- @tags@, if supplied.
getAllProblemData :: [ProblemTag] -> IO (Either String ProblemsResponse)
getAllProblemData ts = getData "/problemset.problems" [("tags", tags)]
  where
    tags = if null ts then Nothing else Just $ BC.pack $ intercalate ";" ts

--------------------------------------------------------------------------------

-- | `getProblems` @tags@ returns a list of @Problem@s containing the @tags@, if
-- provided.
getProblems :: [ProblemTag] -> IO (Either String [Problem])
getProblems ts = fmap prProblems <$> getAllProblemData ts

--------------------------------------------------------------------------------
