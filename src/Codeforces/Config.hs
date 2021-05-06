--------------------------------------------------------------------------------

module Codeforces.Config
    ( UserConfig(..)
    , AuthQuery(..)
    , generateRequestParams
    ) where

import Codeforces.User (Handle)

import qualified Crypto.Hash.SHA512 as SHA512

import Data.Aeson
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX

import Network.HTTP.Simple
import Network.HTTP.Types (renderQuery)

import System.Random

--------------------------------------------------------------------------------

-- | Represents the user's configuration settings.
--
-- The API key must first be generated on
-- <https://codeforces.com/settings/api the Codeforces API page>.
-- An API key consists of two parameters: @key@ and @secret@.
--
-- To use the API key in a request, some parameters need to be generated from
-- this configuration.
--
data UserConfig = UserConfig
    { cfgHandle :: Handle   -- ^ Codeforces handle of the user
    , cfgKey    :: Text     -- ^ First part of the API key
    , cfgSecret :: Text     -- ^ Second part of the API key
    }
    deriving Show

instance FromJSON UserConfig where
    parseJSON = withObject "Config" $ \v ->
        UserConfig <$> (v .: "handle") <*> (v .: "key") <*> (v .: "secret")

instance ToJSON UserConfig where
    toJSON UserConfig {..} =
        object ["handle" .= cfgHandle, "key" .= cfgKey, "secret" .= cfgSecret]

--------------------------------------------------------------------------------

-- | Contains the data needed to make an authorized GET request.
-- See 'generateRequestParams'.
data AuthQuery = AuthQuery
    { aqOriginalQuery :: Query
    , aqMethodName    :: String
    , aqKey           :: Text
    , aqSecret        :: Text
    , aqTime          :: POSIXTime
    , aqRand          :: Int
    }

-- | Returns a query string with extra parameters for authorized access.
--
-- The parameters are of the form:
-- @
-- ?paramI=valueI&apiKey=<key>&time=<time>&apiSig=<rand><hash>
-- @
--
-- Where
--
-- * @<rand>@ is a random 6-digit integer
-- * @<time>@ is current time since epoch in seconds
-- * @<hash>@ is the SHA-512 hashcode of the UTF-8 encoded string:
--
-- @
-- <rand>/<methodName>?paramI=valueI&apiKey=<key>&time=<time>#<secret>
-- @
generateRequestParams :: UserConfig -> String -> Query -> IO Query
generateRequestParams UserConfig {..} path query = do
    (rand :: Int) <- randomRIO (100000, 999999)
    time          <- getPOSIXTime

    let
        authQuery = AuthQuery
            { aqOriginalQuery = query
            , aqMethodName    = path
            , aqKey           = cfgKey
            , aqSecret        = cfgSecret
            , aqTime          = time
            , aqRand          = rand
            }
    let apiSig = (BC.pack . show) rand <> generateHash authQuery

    pure $ concat
        [query, keyAndTimeParams cfgKey time, [("apiSig", Just apiSig)]]

generateHash :: AuthQuery -> ByteString
generateHash AuthQuery {..} = SHA512.hash $ BC.concat
    [ BC.pack (show aqRand)
    , BC.pack aqMethodName
    , renderQuery True $ aqOriginalQuery ++ keyAndTimeParams aqKey aqTime
    , "#"
    , T.encodeUtf8 aqSecret
    ]

-- | A 'Query' consisting the correctly formatted @apiKey@ and POSIX @time@.
keyAndTimeParams :: Text -> POSIXTime -> Query
keyAndTimeParams key time =
    fmap Just
        <$> [("apiKey", T.encodeUtf8 key), ("time", posixToByteString time)]

posixToByteString :: POSIXTime -> ByteString
posixToByteString = BC.pack . (show :: Int -> String) . round

--------------------------------------------------------------------------------
