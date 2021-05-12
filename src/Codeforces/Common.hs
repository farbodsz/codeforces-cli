--------------------------------------------------------------------------------

module Codeforces.Common
    ( ResponseError(..)
    , getData
    , getAuthorizedData
    ) where

import Codeforces.Config

import Data.Aeson

import Network.HTTP.Simple

--------------------------------------------------------------------------------

data CodeforcesStatus = Ok | Failed
    deriving Show

instance FromJSON CodeforcesStatus where
    parseJSON = withText "CodeforcesStatus"
        $ \t -> pure $ if t == "OK" then Ok else Failed

-- | Represents the result object or error comment returned by the API.
--
-- Each successful response from the API contains a "status" field, and either
-- a "result" or "comment" when status is "OK" or "FAILED" respectively.
--
-- These two possibilities are represented by 'ResponseOk' and 'ResponseFail'.
data CodeforcesResponse a = ResponseFail String | ResponseOk a
    deriving Show

instance FromJSON a => FromJSON (CodeforcesResponse a) where
    parseJSON = withObject "CodeforcesResponse" $ \o -> do
        st <- o .: "status"
        case st of
            Ok     -> ResponseOk <$> o .: "result"
            Failed -> ResponseFail <$> o .: "comment"

--------------------------------------------------------------------------------

data ResponseError
    = APIError String
    | JSONError JSONException
    deriving Show

-- | Converts a possible 'CodeforcesResponse' into an either type.
codeforcesDecode
    :: FromJSON a
    => Either JSONException (CodeforcesResponse a)
    -> Either ResponseError a
codeforcesDecode (Left  e1  ) = Left $ JSONError e1
codeforcesDecode (Right resp) = case resp of
    (ResponseFail e2 ) -> Left $ APIError e2
    (ResponseOk   obj) -> Right obj

--------------------------------------------------------------------------------

-- | Host name and the API's base URL, without trailing slash.
baseUrl :: String
baseUrl = "https://codeforces.com/api"

-- | 'getData' @path query@ is a general function for returning some result data
-- from the Codeforces API.
getData :: FromJSON a => String -> Query -> IO (Either ResponseError a)
getData path query = do
    req <- parseRequest (baseUrl ++ path)
    let request = setRequestQueryString query req

    response <- httpJSONEither request
    pure $ codeforcesDecode $ getResponseBody response

-- | 'getAuthorizedData' @config path query@ requests and returnsn some result
-- data that requires authorization.
getAuthorizedData
    :: FromJSON a
    => UserConfig
    -> String
    -> Query
    -> IO (Either ResponseError a)
getAuthorizedData cfg p q = generateRequestParams cfg p q >>= getData p

--------------------------------------------------------------------------------
