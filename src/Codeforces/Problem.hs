--------------------------------------------------------------------------------

module Codeforces.Problem where

import Codeforces.Common

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------

-- | A letter, or letter with digit(s) indicating the problem index in a
-- contest.
type ProblemIndex = Text

type Points = Float

type ProblemTag = Text

data ProblemType = Programming | Question
    deriving Show

instance FromJSON ProblemType where
    parseJSON = withText "ProblemType" $ \case
        "PROGRAMMING" -> pure Programming
        "QUESTION"    -> pure Question
        _             -> fail "Invalid ProblemType"

data Problem = Problem
    { problemContestId :: Maybe Int
    , problemSetName   :: Maybe Text
    , problemIndex     :: ProblemIndex
    , problemName      :: Text
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
    deriving Show

instance FromJSON ProblemsResponse where
    parseJSON =
        withObject "ProblemsResponse"
            $ \v ->
                  ProblemsResponse
                      <$> (v .: "problems")
                      <*> (v .: "problemStatistics")

-- | 'getAllProblemData' @tags@ returns a 'ProblemsResponse' filtered by the
-- @tags@, if supplied.
getAllProblemData :: [ProblemTag] -> IO (Either String ProblemsResponse)
getAllProblemData ts = getData "/problemset.problems" [("tags", tags)]
  where
    tags =
        if null ts then Nothing else Just $ T.encodeUtf8 $ T.intercalate ";" ts

--------------------------------------------------------------------------------

-- | 'getProblems' @tags@ returns a list of 'Problem's containing the @tags@, if
-- provided.
getProblems :: [ProblemTag] -> IO (Either String [Problem])
getProblems ts = fmap prProblems <$> getAllProblemData ts

--------------------------------------------------------------------------------
