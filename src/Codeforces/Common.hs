--------------------------------------------------------------------------------

module Codeforces.Common
    ( getData
    ) where

import Data.Aeson

import Network.HTTP.Simple

--------------------------------------------------------------------------------

data CodeforcesStatus = Ok | Failed
    deriving Show

instance FromJSON CodeforcesStatus where
    parseJSON = withText "CodeforcesStatus"
        $ \t -> pure $ if t == "OK" then Ok else Failed

-- | `CodeforcesResponse` represents the JSON object returned by a GET request
-- to the Codeforces API.
--
-- It always contains a "status", and one of "result" or "comment" depending on
-- whether a "OK" or "FAILED" is returned.
--
data CodeforcesResponse a = ResponseFail String | ResponseOk a
    deriving Show

instance FromJSON a => FromJSON (CodeforcesResponse a) where
    parseJSON = withObject "CodeforcesResponse" $ \o -> do
        st <- o .: "status"
        case st of
            Ok     -> ResponseOk <$> o .: "result"
            Failed -> ResponseFail <$> o .: "comment"

-- | `codeforcesDecode` @response@ decodes the JSON object returned by the API.
-- If the call "FAILED" with a given comment, @Left@ is returned, otherwise
-- @Right@ with the result object.
codeforcesDecode :: FromJSON a => CodeforcesResponse a -> Either String a
codeforcesDecode (ResponseFail e) = Left e
codeforcesDecode (ResponseOk   x) = Right x

--------------------------------------------------------------------------------

-- | Host name and the API's base URL, without trailing slash.
baseUrl :: String
baseUrl = "https://codeforces.com/api"

-- | `getData` @path query@ is a general function for returning some result data
-- from the Codeforces API.
getData :: FromJSON a => String -> Query -> IO (Either String a)
getData path query = do
    req <- parseRequest (baseUrl ++ path)
    let request = setRequestQueryString query req

    response <- httpJSON request
    pure $ codeforcesDecode $ getResponseBody response

--------------------------------------------------------------------------------
