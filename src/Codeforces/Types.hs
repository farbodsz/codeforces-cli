--------------------------------------------------------------------------------

-- | Common types used across the application.
--
module Codeforces.Types where

import Data.Text (Text)

--------------------------------------------------------------------------------

-- | Codeforces user handle.
type Handle = Text

-- | Number of points gained for a submission or across a contest.
type Points = Float

-- | A letter, or letter with digit(s) indicating the problem index in a
-- contest.
type ProblemIndex = Text

--------------------------------------------------------------------------------
