--------------------------------------------------------------------------------

module Codeforces.Response
    ( ResponseError(..)
    , responseErrorMsg
    , getData
    , getAuthorizedData
    ) where

import           Codeforces.Config
import           Codeforces.Logging

import           Control.Exception              ( try )

import           Data.Aeson
import           Data.Maybe

import           Network.HTTP.Client
import           Network.HTTP.Simple

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
-- These two possibilities are represented by @ResponseOk@ and @ResponseFail@.
--
data CodeforcesResponse a = ResponseFail String | ResponseOk a
    deriving Show

instance FromJSON a => FromJSON (CodeforcesResponse a) where
    parseJSON = withObject "CodeforcesResponse" $ \o -> do
        st <- o .: "status"
        case st of
            Ok     -> ResponseOk <$> o .: "result"
            Failed -> ResponseFail <$> o .: "comment"

--------------------------------------------------------------------------------

-- | An error that could occur during the retrieval and inital parsing of data
-- from the Codeforces API.
data ResponseError
    = ApiFail String
    -- ^ Corresponds to a @ResponseFail@ from the Codeforces API with the fail
    -- comment.
    | JsonError JSONException
    -- ^ Wrapper around 'JSONException', used if the successful JSON response
    -- could not be parsed.
    | HttpError HttpException
    -- ^ Wrapper around 'HttpException', used for situations like a failed
    -- connection.
    deriving Show

-- | Converts a 'ResponseError' to a friendly error message to display, and
-- details that can be logged separately.
responseErrorMsg :: ResponseError -> ErrorLog
responseErrorMsg (ApiFail   e) = mkErrorLog e
responseErrorMsg (JsonError e) = "Couldn't parse result" <~> show e
responseErrorMsg (HttpError e) = case e of
    HttpExceptionRequest _ content -> case content of
        StatusCodeException resp _ ->
            mkErrorLog $ "HTTP " <> show (getResponseStatusCode resp)
        TooManyRedirects resps -> "Too many redirects" <~> show resps
        ConnectionTimeout      -> mkErrorLog "Connection timeout"
        ConnectionFailure e2   -> "Connection failure" <~> show e2
        other                  -> "HTTP error" <~> show other
    InvalidUrlException url reason ->
        "Invalid URL" <~> (url <> "\nfor " <> reason)

--------------------------------------------------------------------------------

-- | Converts a possible 'CodeforcesResponse' into an either type.
codeforcesDecode
    :: FromJSON a
    => Either JSONException (CodeforcesResponse a)
    -> Either ResponseError a
codeforcesDecode (Left  e1  ) = Left $ JsonError e1
codeforcesDecode (Right resp) = case resp of
    (ResponseFail e2 ) -> Left $ ApiFail e2
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
    let request = setRequestQueryString (catQuery query) req

    eresponse <- try $ httpJSONEither request
    pure $ case eresponse of
        Left  e    -> Left $ HttpError e
        Right resp -> codeforcesDecode $ getResponseBody resp

-- | 'getAuthorizedData' @config path query@ requests and returns some result
-- data that requires authorization.
getAuthorizedData
    :: FromJSON a
    => UserConfig
    -> String
    -> Query
    -> IO (Either ResponseError a)
getAuthorizedData cfg p q = generateRequestParams cfg p q >>= getData p

--------------------------------------------------------------------------------

-- | Takes a list of 'QueryItem's and returns those that have a @Just@ parameter
-- value.
--
-- By default, @Nothing@ items in 'Query' are parsed into "&a&b" format. The
-- Codeforces API seems to be inconsistent with how it interprets requests like
-- this. Most notably, the @contest.standings@ endpoint returns an empty list of
-- ranklist rows when @Nothing@ is passed, but all ranklist rows when completely
-- omitted.
--
catQuery :: Query -> Query
catQuery = filter (isJust . snd)

--------------------------------------------------------------------------------
