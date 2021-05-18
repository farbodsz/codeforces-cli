--------------------------------------------------------------------------------

module Codeforces.RatingChange where

import Codeforces.Types

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

--------------------------------------------------------------------------------

data RatingChange = RatingChange
    { rcContestId        :: ContestId
    , rcContestName      :: Text
    , rcHandle           :: Handle
    , rcRank             :: Int
    , rcRatingUpdateDate :: UTCTime
    , rcOldRating        :: Rating
    , rcNewRating        :: Rating
    }
    deriving Show

instance FromJSON RatingChange where
    parseJSON = withObject "RatingChange" $ \v ->
        RatingChange
            <$> (v .: "contestId")
            <*> (v .: "contestName")
            <*> (v .: "handle")
            <*> (v .: "rank")
            <*> (posixSecondsToUTCTime <$> (v .: "ratingUpdateTimeSeconds"))
            <*> (v .: "oldRating")
            <*> (v .: "newRating")

--------------------------------------------------------------------------------
