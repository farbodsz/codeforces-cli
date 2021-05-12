--------------------------------------------------------------------------------

module Codeforces.RatingChange where

import Codeforces.User (Handle)

import Data.Aeson
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

--------------------------------------------------------------------------------

data RatingChange = RatingChange
    { rcContestId        :: Int
    , rcContestName      :: Text
    , rcHandle           :: Handle
    , rcRank             :: Int
    , rcRatingUpdateDate :: UTCTime
    , rcOldRating        :: Int
    , rcNewRating        :: Int
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
