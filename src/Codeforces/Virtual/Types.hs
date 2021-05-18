--------------------------------------------------------------------------------

module Codeforces.Virtual.Types where

import Codeforces.Party
import Codeforces.Problem (Points)
import Codeforces.User

--------------------------------------------------------------------------------

virtualHandle :: Handle
virtualHandle = "VIRTUAL_USER"

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
    , vuRating  :: Int      -- ^ Current rating of the user.
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
    , contestantRating :: Int
    }
    deriving (Eq, Show)

-- | A virtual participation result
data VirtualResult = VirtualResult
    { virtualUser  :: User
    , virtualDelta :: Delta
    , virtualSeed  :: Seed
    }
    deriving Show

--------------------------------------------------------------------------------
