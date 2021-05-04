--------------------------------------------------------------------------------

module Codeforces.Contest where

import Codeforces.Common

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX

--------------------------------------------------------------------------------

data ScoringType = ScoringCF | ScoringIOI | ScoringICPC
    deriving (Show, Eq)

instance FromJSON ScoringType where
    parseJSON = withText "ScoringType" $ \case
        "CF"   -> pure ScoringCF
        "IOI"  -> pure ScoringIOI
        "ICPC" -> pure ScoringICPC
        _      -> fail "Invalid Scoring Type"

data ContestPhase = Before | Coding | PendingSystemTest | Finished
    deriving Show

instance FromJSON ContestPhase where
    parseJSON = withText "ContestPhase" $ \case
        "BEFORE"              -> pure Before
        "CODING"              -> pure Before
        "PENDING_SYSTEM_TEST" -> pure PendingSystemTest
        "FINISHED"            -> pure Finished
        _                     -> fail "Invalid Contest Phase"

data Contest = Contest
    { contestId        :: Int
    , contestName      :: Text
    , contestType      :: ScoringType
    , contestPhase     :: ContestPhase
    , contestFrozen    :: Bool
    , contestDuration  :: DiffTime
    , contestStartTime :: Maybe UTCTime
    }
    deriving Show

instance FromJSON Contest where
    parseJSON = withObject "Contest" $ \v ->
        let
            durationSeconds = (v .: "durationSeconds")
            startTimePosix  = (v .:? "startTimeSeconds")
        in
            Contest
            <$> (v .: "id")
            <*> (v .: "name")
            <*> (v .: "type")
            <*> (v .: "phase")
            <*> (v .: "frozen")
            <*> (secondsToDiffTime <$> durationSeconds)
            <*> (fmap posixSecondsToUTCTime <$> startTimePosix)

--------------------------------------------------------------------------------

getContests :: Bool -> IO (Either String [Contest])
getContests isGym =
    getData "/contest.list" [("gym", Just (BC.pack $ show isGym))]

--------------------------------------------------------------------------------
