--------------------------------------------------------------------------------

module Codeforces.Virtual.Types where

import Codeforces.Types.Common
import Codeforces.Types.Party

import Data.List
import qualified Data.Map as M

--------------------------------------------------------------------------------

virtualHandle :: Handle
virtualHandle = Handle "VIRTUAL_USER"

-- | A 'Party' representing the user's virtual participation.
virtualParty :: Party
virtualParty = Party
    { partyContestId       = Nothing
    , partyMembers         = [Member virtualHandle]
    , partyParticipantType = Virtual
    , partyTeamId          = Nothing
    , partyTeamName        = Nothing
    , partyIsGhost         = False
    , partyRoom            = Nothing
    , partyStartTime       = Nothing
    }

-- | Represents the virtual participation of the user in this contest.
data VirtualUser = VirtualUser
    { vuPoints  :: Points   -- ^ Points scored in the virtual contest.
    , vuPenalty :: Int      -- ^ User's penalty in the virtual contest.
    , vuRating  :: Rating   -- ^ Current rating of the user.
    }
    deriving Show

--------------------------------------------------------------------------------

-- | Difference in rating between a user's current rating, and their rating
-- following this contest.
type Delta = Int

-- | The seed is the expected ranking for each participant before the contest
-- begins.
--
-- A contestant's rating increases should they perform better than their seed,
-- and decreases should they perform worse.
--
type Seed = Float

-- | The participation of a user in a contest.
data Contestant = Contestant
    { contestantParty  :: Party
    , contestantRank   :: Int
    , contestantPoints :: Points
    , contestantRating :: Rating
    }
    deriving (Eq, Show)

-- | Finds a single contestant from the given 'Party', or @Nothing@ if none
-- found.
findContestant :: Party -> [Contestant] -> Maybe Contestant
findContestant p = find ((p ==) . contestantParty)

data ContestResults = ContestResults
    { crContestants :: [Contestant]
    , crDeltas      :: M.Map Party Delta
    , crSeeds       :: M.Map Rating Seed
    }
    deriving Show

-- | A virtual participation result
data VirtualResult = VirtualResult
    { virtualRank  :: Int
    , virtualDelta :: Delta
    , virtualSeed  :: Seed
    }
    deriving Show

--------------------------------------------------------------------------------
