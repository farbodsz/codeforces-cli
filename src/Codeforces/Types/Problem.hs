--------------------------------------------------------------------------------

module Codeforces.Types.Problem where

import Codeforces.Types.Common

import Data.Aeson
import Data.Text (Text)

--------------------------------------------------------------------------------

type ProblemTag = Text

data ProblemType = Programming | Question
    deriving Show

instance FromJSON ProblemType where
    parseJSON = withText "ProblemType" $ \case
        "PROGRAMMING" -> pure Programming
        "QUESTION"    -> pure Question
        _             -> fail "Invalid ProblemType"

data Problem = Problem
    { problemContestId :: Maybe ContestId
    -- ^ ID of /a/ contest containing the problem.
    --
    -- Note that a problem may appear in multiple contests (such as Div. 1 and
    -- Div. 2 variants of a contest), but this field only contains one.
    --
    , problemSetName   :: Maybe Text
    , problemIndex     :: ProblemIndex
    , problemName      :: Text
    , problemType      :: ProblemType
    , problemPoints    :: Maybe Points
    , problemRating    :: Maybe Rating
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
    { pStatContestId    :: Maybe ContestId
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

--------------------------------------------------------------------------------
