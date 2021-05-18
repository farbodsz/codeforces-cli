--------------------------------------------------------------------------------

-- | Common types used across the application.
--
module Codeforces.Types where

import Data.Aeson
import Data.Text (Text)

--------------------------------------------------------------------------------

-- | ID of the contest.
--
-- Not to be confused with contest round number. The ID appears in the contest
-- URL, for example in: <https://codeforces.com/contest/566/status>.
--
newtype ContestId = ContestId { unContestId :: Int }
    deriving (Eq, Ord, Show)

instance FromJSON ContestId where
    parseJSON v = ContestId <$> parseJSON v

-- | Codeforces user handle.
newtype Handle = Handle { unHandle :: Text }
    deriving (Eq, Ord, Show)

instance FromJSON Handle where
    parseJSON v = Handle <$> parseJSON v

instance ToJSON Handle where
    toJSON = toJSON . unHandle

-- | Number of points gained for a submission or across a contest.
type Points = Float

-- | A letter, or letter with digit(s) indicating the problem index in a
-- contest.
type ProblemIndex = Text

-- | User or problem rating.
type Rating = Int

--------------------------------------------------------------------------------
