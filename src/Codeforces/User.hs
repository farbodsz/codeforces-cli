--------------------------------------------------------------------------------

module Codeforces.User where

import Codeforces.Config
import Codeforces.Common

import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
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
            <*> (fromMaybe 0 <$> v .:? "rating")
            <*> (fromMaybe 0 <$> v .:? "maxRating")
            <*> (v .:? "city")
            <*> (v .:? "country")
            <*> (v .:? "organization")
            <*> (v .: "friendOfCount")

--------------------------------------------------------------------------------

-- | 'getUser' @handle@ returns the @User@ with the given @handle@
getUser :: Handle -> IO (Either String User)
getUser h = fmap head <$> getUsers [h]

-- | 'getUsers' @handles@ returns a list of @User@s with the given @handles@
getUsers :: [Handle] -> IO (Either String [User])
getUsers hs = getData
    "/user.info"
    [("handles", Just $ T.encodeUtf8 $ T.intercalate ";" hs)]

-- 'getFriends' @config@ returns the handles of the friends of the currently
-- authenticated user.
getFriends :: UserConfig -> IO (Either String [Handle])
getFriends cfg = getAuthorizedData cfg "/user.friends" []

--------------------------------------------------------------------------------
