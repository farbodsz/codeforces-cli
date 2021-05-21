--------------------------------------------------------------------------------

-- | Provides a way of dealing with errors encountered during the retrieval or
-- processing of data from the Codeforces API.
--
module Error where

import Codeforces.API

--------------------------------------------------------------------------------

-- | An error that may occur in this application.
class CodeforcesError e where
    showE :: e -> String

instance CodeforcesError ResponseError where
    showE (APIError  e) = show e
    showE (JSONError e) = show e

--------------------------------------------------------------------------------

-- | 'handleE' @m@ runs the computation @m@ that may produce a
-- 'CodeforcesError'. If an error is encountered, its error message is printed.
handleE :: CodeforcesError e => IO (Either e a) -> IO ()
handleE m = m >>= \case
    Left  e -> putStrLn $ showE e
    Right _ -> pure ()

--------------------------------------------------------------------------------
