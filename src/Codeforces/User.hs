--------------------------------------------------------------------------------

module Codeforces.User where

import Codeforces.Common

import Data.Aeson
import qualified Data.ByteString.Char8 as BC

--------------------------------------------------------------------------------

type Handle = String

data User = User
    { handle        :: Handle
    , firstName     :: Maybe String
    , lastName      :: Maybe String
    , rating        :: Int
    , maxRating     :: Int
    , city          :: Maybe String
    , country       :: Maybe String
    , organization  :: Maybe String
    , friendOfCount :: Int
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
getUser h = fmap head <$> getData "/user.info" [("handles", Just (BC.pack h))]

--------------------------------------------------------------------------------
