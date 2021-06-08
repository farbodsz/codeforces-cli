--------------------------------------------------------------------------------

-- | Provides a way of dealing with errors encountered during the retrieval or
-- processing of data from the Codeforces API.
--
module Codeforces.Error where

import qualified Codeforces.API as API

--------------------------------------------------------------------------------

-- | An error that may occur in this application.
data CodeforcesError
    = ResponseError API.ResponseError
    | StandingsEmpty
    | StandingsWithFriendsEmpty
    | VirtualNoResult

showE :: CodeforcesError -> String
showE (ResponseError e) = show e
showE StandingsEmpty    = "Standings empty."
showE StandingsWithFriendsEmpty =
    "Neither you nor your friends participated in this contest."
showE VirtualNoResult =
    "An unexpected error occurred.\nYour rating change could not be calculated."

--------------------------------------------------------------------------------

-- | 'handleE' @m@ runs the computation @m@ that may produce a
-- 'CodeforcesError'. If an error is encountered, its error message is printed.
handleE :: IO (Either CodeforcesError a) -> IO ()
handleE m = m >>= \case
    Left  e -> putStrLn $ showE e
    Right _ -> pure ()

--------------------------------------------------------------------------------
