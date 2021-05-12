--------------------------------------------------------------------------------

module Codeforces.User where

import Data.Aeson
import Data.Maybe
import Data.Text (Text)

--------------------------------------------------------------------------------

type Handle = Text

data User = User
    { userHandle        :: Handle
    , userFirstName     :: Maybe Text
    , userLastName      :: Maybe Text
    , userRating        :: Int
    , userMaxRating     :: Int
    , userCity          :: Maybe Text
    , userCountry       :: Maybe Text
    , userOrganization  :: Maybe Text
    , userFriendOfCount :: Int
    }
    deriving Show

instance FromJSON User where
    parseJSON = withObject "User" $ \v ->
        User
            <$> (v .: "handle")
            <*> (v .:? "firstName")
            <*> (v .:? "lastName")
            <*> (fromMaybe 0 <$> v .:? "rating")
            <*> (fromMaybe 0 <$> v .:? "maxRating")
            <*> (v .:? "city")
            <*> (v .:? "country")
            <*> (v .:? "organization")
            <*> (v .: "friendOfCount")

--------------------------------------------------------------------------------
