--------------------------------------------------------------------------------

module Codeforces.Types.Party where

import Codeforces.Types.Common

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

--------------------------------------------------------------------------------

-- | Member of a party.
data Member = Member
    { memberHandle :: Handle
    }
    deriving (Eq, Ord, Show)

instance FromJSON Member where
    parseJSON = withObject "Member" $ \v -> Member <$> (v .: "handle")

data ParticipantType
    = Contestant
    | Practice
    | Virtual
    | Manager
    | OutOfCompetition
    deriving (Eq, Ord, Show)

instance FromJSON ParticipantType where
    parseJSON = withText "ParticipantType" $ \case
        "CONTESTANT"         -> pure Contestant
        "PRACTICE"           -> pure Practice
        "VIRTUAL"            -> pure Virtual
        "MANAGER"            -> pure Manager
        "OUT_OF_COMPETITION" -> pure OutOfCompetition
        _                    -> fail "Invalid ParticipantType"

-- | Represents a party, participating in a contest.
data Party = Party
    { partyContestId       :: Maybe ContestId
    , partyMembers         :: [Member]
    , partyParticipantType :: ParticipantType
    , partyTeamId          :: Maybe Int
    , partyTeamName        :: Maybe Text
    , partyIsGhost         :: Bool
    , partyRoom            :: Maybe Int
    , partyStartTime       :: Maybe UTCTime
    }
    deriving (Eq, Ord, Show)

instance FromJSON Party where
    parseJSON = withObject "Party" $ \v ->
        Party
            <$> (v .:? "contestId")
            <*> (v .: "members")
            <*> (v .: "participantType")
            <*> (v .:? "teamId")
            <*> (v .:? "teamName")
            <*> (v .: "ghost")
            <*> (v .:? "room")
            <*> (fmap posixSecondsToUTCTime <$> v .:? "startTimeSeconds")

--------------------------------------------------------------------------------
