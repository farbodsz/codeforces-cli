--------------------------------------------------------------------------------

module Codeforces.Types.Submission where

import           Codeforces.Types.Common
import           Codeforces.Types.Party         ( Party )
import           Codeforces.Types.Problem       ( Problem )

import           Data.Aeson
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )

--------------------------------------------------------------------------------

data Verdict
    = Failed
    | Ok
    | Partial
    | CompilationError
    | RuntimeError
    | WrongAnswer
    | PresentationError
    | TimeLimitExceeded
    | MemoryLimitExceeded
    | IdlenessLimitExceeded
    | SecurityViolated
    | Crashed
    | InputPreparationCrashed
    | Challenged
    | Skipped
    | Testing
    | Rejected
    deriving (Show, Eq)

instance FromJSON Verdict where
    parseJSON = withText "Verdict" $ \case
        "FAILED"                    -> pure Failed
        "OK"                        -> pure Ok
        "PARTIAL"                   -> pure Partial
        "COMPILATION_ERROR"         -> pure CompilationError
        "RUNTIME_ERROR"             -> pure RuntimeError
        "WRONG_ANSWER"              -> pure WrongAnswer
        "PRESENTATION_ERROR"        -> pure PresentationError
        "TIME_LIMIT_EXCEEDED"       -> pure TimeLimitExceeded
        "MEMORY_LIMIT_EXCEEDED"     -> pure MemoryLimitExceeded
        "IDLENESS_LIMIT_EXCEEDED"   -> pure IdlenessLimitExceeded
        "SECURITY_VIOLATED"         -> pure SecurityViolated
        "CRASHED"                   -> pure Crashed
        "INPUT_PREPARATION_CRASHED" -> pure InputPreparationCrashed
        "CHALLENGED"                -> pure Challenged
        "SKIPPED"                   -> pure Skipped
        "TESTING"                   -> pure Testing
        "REJECTED"                  -> pure Rejected
        _                           -> fail "Invalid Verdict"

-- | 'verdictText' @verdict@ returns a user-friendly text representation of the
-- Verdict.
verdictText :: Verdict -> Text
verdictText Failed                  = "Failed"
verdictText Ok                      = "Ok"
verdictText Partial                 = "Partial"
verdictText CompilationError        = "Compilation error"
verdictText RuntimeError            = "Runtime error"
verdictText WrongAnswer             = "Wrong answer"
verdictText PresentationError       = "Presentation error"
verdictText TimeLimitExceeded       = "Time limit exceeded"
verdictText MemoryLimitExceeded     = "Memory limit exceeded"
verdictText IdlenessLimitExceeded   = "Idleness limit exceeded"
verdictText SecurityViolated        = "Security violated"
verdictText Crashed                 = "Crashed"
verdictText InputPreparationCrashed = "Input preparation crashed"
verdictText Challenged              = "Challenged"
verdictText Skipped                 = "Skipped"
verdictText Testing                 = "Testing"
verdictText Rejected                = "Rejected"

-- | Testset used for judging a submission.
data Testset
    = Samples
    | Pretests
    | Tests
    | Challenges
    deriving Show

instance FromJSON Testset where
    parseJSON = withText "Testset" $ \case
        "SAMPLES"    -> pure Samples
        "PRETESTS"   -> pure Pretests
        "TESTS"      -> pure Tests
        "CHALLENGES" -> pure Challenges
        x            -> if "TESTS" `T.isPrefixOf` x
            then pure Tests
            else fail $ "Invalid Testset: " ++ T.unpack x

data Submission = Submission
    { submissionId                  :: Int
    , submissionContestId           :: Maybe ContestId
      -- | Time when the solution was submitted.
    , submissionTime                :: UTCTime
      -- | The time passed after the start of the contest (or a virtual start
      -- for virtual parties), before the submission.
    , submissionRelativeTime        :: DiffTime
    , submissionProblem             :: Problem
    , submissionAuthor              :: Party
    , submissionProgrammingLanguage :: Text
    , submissionVerdict             :: Maybe Verdict
    , submissionTestset             :: Testset
    , submissionPassedTestCount     :: Int
      -- | Maximum time (in ms) consumed by the submission for one test.
    , submissionTimeConsumed        :: Int
      -- | Maximum memory (in bytes) consumed by the submission for one test.
    , submissionMemoryConsumed      :: Int
      -- | Number of scored points for IOI-like contests.
    , submissionPoints              :: Maybe Points
    }
    deriving Show

instance FromJSON Submission where
    parseJSON = withObject "Submission" $ \v ->
        Submission
            <$> (v .: "id")
            <*> (v .:? "contestId")
            <*> (posixSecondsToUTCTime <$> v .: "creationTimeSeconds")
            <*> (secondsToDiffTime <$> v .: "relativeTimeSeconds")
            <*> (v .: "problem")
            <*> (v .: "author")
            <*> (v .: "programmingLanguage")
            <*> (v .: "verdict")
            <*> (v .: "testset")
            <*> (v .: "passedTestCount")
            <*> (v .: "timeConsumedMillis")
            <*> (v .: "memoryConsumedBytes")
            <*> (v .:? "points")

--------------------------------------------------------------------------------
