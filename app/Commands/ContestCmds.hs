--------------------------------------------------------------------------------

-- | Contest-related commands.
module Commands.ContestCmds
    ( contestList
    , contestInfo
    , openContest
    ) where

import Codeforces

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import Error
import Format
import Options
import Table
import Watcher

import Web.Browser

--------------------------------------------------------------------------------

contestList :: ContestOpts -> IO ()
contestList ContestOpts {..} = handleE $ runExceptT $ do
    contests <- ExceptT $ getContests optIsGym
    now      <- lift getCurrentTime

    let headers = [("#", 4), ("Name", 50), ("Date", 16), ("Duration", 10)]
        rows    = map
            (\Contest {..} ->
                plainCell
                    <$> [ showText $ unContestId contestId
                        , contestName
                        , fmtStartTime contestStartTime
                        , fmtDuration contestDuration
                        ]
            )
            (filterContests optIsPast now contests)

    lift $ mapM_ T.putStrLn $ makeTable headers rows

-- | 'filterContests' @onlyPast currentTime@ filters and orders a list of
-- contests depending on whether past or upcoming should be displayed.
filterContests :: Bool -> UTCTime -> [Contest] -> [Contest]
filterContests past now = if past
    then filter isContestPast
    else reverse . filter (not . isContestPast)
  where
    isContestPast :: Contest -> Bool
    isContestPast c = case contestStartTime c of
        Nothing -> False
        Just t  -> t < now

fmtStartTime :: Maybe UTCTime -> Text
fmtStartTime =
    maybe "" (T.pack . formatTime defaultTimeLocale "%H:%M  %d-%b-%y")

fmtDuration :: DiffTime -> Text
fmtDuration = T.pack . formatTime defaultTimeLocale "%h:%0M hrs"

--------------------------------------------------------------------------------

contestInfo :: ContestId -> UserConfig -> InfoOpts -> IO ()
contestInfo cId cfg opts =
    handleWatch (optInfoWatch opts) (contestInfoTable cId cfg opts)

-- | 'contestInfoTable' @problems submissions statistics@ fetches data about the
-- contest and constructs a table of its problems.
--
-- The table includes problem statistics, and if the user has made a submission,
-- their submission verdict for the problem.
--
contestInfoTable
    :: ContestId -> UserConfig -> InfoOpts -> IO (Either ResponseError Table)
contestInfoTable cId cfg opts = runExceptT $ do
    let handle = fromMaybe (cfgHandle cfg) (optHandle opts)

    ps <- ExceptT $ getContestProblems cId
    statMap <- ExceptT $ fmap problemStatsMap <$> getProblemStats []
    subMap <- ExceptT $ fmap submissionsMap <$> getContestSubmissions cId handle

    let headers =
            [ ("#"      , 2)
            , ("Problem", 30)
            , ("Verdict", 35)
            , ("Time"   , 7)
            , ("Memory" , 8)
            , ("Solved" , 7)
            ]
        rows = map
            (\Problem {..} ->
                let
                    mSub   = M.lookup problemIndex subMap
                    mStats = M.lookup problemIndex statMap
                in
                    [ plainCell problemIndex
                    , plainCell problemName
                    , contestSubmissionCell mSub
                    , plainCell $ maybeTimeTaken mSub
                    , plainCell $ maybeMemTaken mSub
                    , plainCell $ maybeSolved mStats
                    ]
            )
            ps

    pure $ makeTable headers rows
  where
    maybeTimeTaken = maybe "-" (fmtTimeConsumed . submissionTimeConsumed)
    maybeMemTaken  = maybe "-" (fmtMemoryConsumed . submissionMemoryConsumed)
    maybeSolved    = maybe "" (("x" <>) . showText . pStatSolvedCount)

-- | Shows the verdict of a contest submission.
contestSubmissionCell :: Maybe Submission -> Cell
contestSubmissionCell Nothing                = plainCell "-"
contestSubmissionCell (Just Submission {..}) = verdictCell
    submissionTestset
    submissionPassedTestCount
    submissionPoints
    submissionVerdict

-- | 'problemStatsMap' @stats@ computes a map of each problem's index to the
-- corresponding 'ProblemStats' for it.
problemStatsMap :: [ProblemStats] -> M.Map ProblemIndex ProblemStats
problemStatsMap = M.fromList . map (pStatProblemIndex >>= (,))

-- | 'submissionsMap' @submissions@ computes a map of each problem's index to
-- the most recent submission for it.
submissionsMap :: [Submission] -> M.Map ProblemIndex Submission
submissionsMap =
    M.fromListWith (const id) . map (problemIndex . submissionProblem >>= (,))

--------------------------------------------------------------------------------

-- | 'openContest' @contestId@ opens the URL to the specified contest in the
-- user's preferred web browser.
openContest :: ContestId -> IO ()
openContest cId =
    openBrowser ("https://codeforces.com/contest/" <> show cId) >> pure ()

--------------------------------------------------------------------------------
