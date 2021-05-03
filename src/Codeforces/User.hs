--------------------------------------------------------------------------------

module Codeforces.User where

import Codeforces.Common

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text.Encoding as T

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
            <*> (v .: "rating")
            <*> (v .: "maxRating")
            <*> (v .:? "city")
            <*> (v .:? "country")
            <*> (v .:? "organization")
            <*> (v .: "friendOfCount")

--------------------------------------------------------------------------------

-- | `getUser` @handle@ returns the @User@ with the given @handle@
getUser :: Handle -> IO (Either String User)
getUser h = fmap head <$> getData "/user.info" [("handles", Just (T.encodeUtf8 h))]

--------------------------------------------------------------------------------
