--------------------------------------------------------------------------------

-- | Provides a way of dealing with errors encountered during the retrieval or
-- processing of data from the Codeforces API.
--
module Codeforces.Error where

import           Codeforces.Logging
import qualified Codeforces.Response           as R

--------------------------------------------------------------------------------

-- | An error that may occur in this application.
data CodeforcesError
    = ResponseError R.ResponseError
    | StandingsEmpty
    | StandingsWithFriendsEmpty
    | VirtualNoResult

-- | Returns a human-friendly error message with error details.
showE :: CodeforcesError -> ErrorLog
showE (ResponseError e) = R.responseErrorMsg e
showE StandingsEmpty    = mkErrorLog "Standings empty."
showE StandingsWithFriendsEmpty =
    mkErrorLog "Neither you nor your friends participated in this contest."
showE VirtualNoResult =
    mkErrorLog
        "An unexpected error occurred.\
            \ Your rating change could not be calculated."

--------------------------------------------------------------------------------

-- | 'handleE' @m@ runs the computation @m@ that may produce a
-- 'CodeforcesError'. If an error is encountered, its error message is printed.
handleE :: IO (Either CodeforcesError a) -> IO ()
handleE m = m >>= \case
    Left  e -> putStrLn . elErrorMsg . showE $ e
    Right _ -> pure ()

--------------------------------------------------------------------------------
