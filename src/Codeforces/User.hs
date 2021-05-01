--------------------------------------------------------------------------------

module Codeforces.User where

import Codeforces.Common

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.List (intercalate)

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

-- | `getUsers` @handles@ returns a list of users given a list of handles
getUsers :: [Handle] -> IO (Either String [User])
getUsers hs = getObjects "/user.info" [("handles", Just handles)]
    where handles = BC.pack $ intercalate ";" hs

--------------------------------------------------------------------------------
