--------------------------------------------------------------------------------

module Codeforces.RatingChange where

import Codeforces.Common
import Codeforces.User (Handle)

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

--------------------------------------------------------------------------------

data RatingChange = RatingChange
    { rcContestId        :: Int
    , rcContestName      :: String
    , rcHandle           :: Handle
    , rcRank             :: Int
    , rcRatingUpdateDate :: UTCTime
    , rcOldRating        :: Int
    , rcNewRating        :: Int
    }

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

-- | `getUserRatingHistory` @handle@ returns a list of @RatingChange@s for the
-- requested user
getUserRatingHistory :: Handle -> IO (Either String [RatingChange])
getUserRatingHistory h = getData "/user.rating" [("handle", Just (BC.pack h))]

--------------------------------------------------------------------------------
