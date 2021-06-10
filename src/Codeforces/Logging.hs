--------------------------------------------------------------------------------

-- | Utility functions for producing and writing error logs.
--
module Codeforces.Logging where

--------------------------------------------------------------------------------

-- | Contains a brief, user-friendly error message and potential loggable data.
data ErrorLog = ErrorLog
    { elErrorMsg :: String
    , elErrorLog :: Maybe String
    }
    deriving Show

-- | Produces an error and log using a single error message.
mkErrorLog :: String -> ErrorLog
mkErrorLog = flip ErrorLog Nothing

-- | Takes an error message and error details and produces a pair of the message
-- and full error details.
(<~>) :: String -> String -> ErrorLog
(<~>) msg details = ErrorLog msg (Just $ msg ++ ": " ++ details)

--------------------------------------------------------------------------------
