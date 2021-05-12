--------------------------------------------------------------------------------

module Main where

import Codeforces hiding (RankColor(..))
import qualified Codeforces.Rank as R

import Commands
import Config

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import System.Console.ANSI

import Table

import Web.Browser (openBrowser)

--------------------------------------------------------------------------------

main :: IO ()
main = do
    command <- parseCommands
    config  <- loadConfig

    case command of
        -- List/tabulate data
        ContestsCmd opts      -> contestList opts
        InfoCmd cId opts      -> contestInfo cId config opts
        ProblemsCmd opts      -> problemList opts
        StandingsCmd cId opts -> standingsList cId config opts

        -- User-related commands
        UserCmd    h          -> userInfo h
        RatingsCmd h          -> userRatings h
        StatusCmd h opts      -> userStatus h opts
        FriendsCmd            -> userFriends config

        -- Miscellaneous
        SetupCmd              -> setupConfig
        OpenCmd cId           -> openContest cId

--------------------------------------------------------------------------------

contestList :: ContestOpts -> IO ()
contestList ContestOpts {..} = do
    contests <- getContests optIsGym
    now      <- getCurrentTime
    either printError (printContests . filterContests optIsPast now) contests

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

printContests :: [Contest] -> IO ()
printContests cs = forM_ (makeTable headers rows) T.putStrLn
  where
    headers = [("#", 4), ("Name", 50), ("Date", 16), ("Duration", 10)]
    rows    = map
        (\Contest {..} ->
            plainCell
                <$> [ showText contestId
                    , contestName
                    , fmtStartTime contestStartTime
                    , fmtDuration contestDuration
                    ]
        )
        cs
    fmtStartTime =
        maybe "" (T.pack . formatTime defaultTimeLocale "%H:%M  %d-%b-%y")
    fmtDuration = T.pack . formatTime defaultTimeLocale "%h:%0M hrs"

--------------------------------------------------------------------------------

contestInfo :: Int -> UserConfig -> InfoOpts -> IO ()
contestInfo cId cfg opts =
    runExceptT (printContestInfo cId cfg opts) >>= either printError pure

printContestInfo
    :: Int -> UserConfig -> InfoOpts -> ExceptT ResponseError IO ()
printContestInfo cId cfg opts = do
    let handle = fromMaybe (cfgHandle cfg) (optHandle opts)

    pr    <- ExceptT $ getAllProblemData []
    allSs <- ExceptT $ getContestSubmissions cId handle

    let ps      = contestProblems cId (prProblems pr)
    let statMap = contestStats cId (prStats pr)
    let subMap  = contestSubmissions allSs

    lift $ printContestInfoTable ps subMap statMap

-- | 'contestProblems' @contestId@ returns the problems in contest, sorted by
-- problem index ascending.
contestProblems :: Int -> [Problem] -> [Problem]
contestProblems cId = sortBy (compare `on` problemIndex)
    . filter ((Just cId ==) . problemContestId)

-- | contestStats @contestId stats@ computes all problems in the contest with
-- @contestId@ returns a map of each contest problem's index to its statistics.
contestStats :: Int -> [ProblemStats] -> M.Map ProblemIndex ProblemStats
contestStats cId stats =
    let
        inContest = filter ((Just cId ==) . pStatContestId) stats
        pairs     = map (\x -> (pStatProblemIndex x, x)) inContest
    in M.fromList pairs

-- | 'contestSubmissions' @submissions@ computes a map of each problem's index
-- to the most recent submission for it.
contestSubmissions :: [Submission] -> M.Map ProblemIndex Submission
contestSubmissions ss =
    let pairs = map (\s -> (problemIndex (submissionProblem s), s)) ss
    in M.fromListWith (const id) pairs

-- | 'printContestInfoTable' @problems submissions statistics@ prints a table
-- of problems in this contest, each of which with the problem statistics, and
-- if the user has made a submission, their submission verdict for the problem.
printContestInfoTable
    :: [Problem]
    -> M.Map ProblemIndex Submission
    -> M.Map ProblemIndex ProblemStats
    -> IO ()
printContestInfoTable ps subMap statMap = forM_
    (makeTable headers rows)
    T.putStrLn
  where
    headers =
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

    maybeTimeTaken = maybe "-" (fmtTimeConsumed . submissionTimeConsumed)
    maybeMemTaken  = maybe "-" (fmtMemoryConsumed . submissionMemoryConsumed)
    maybeSolved    = maybe "" (("x" <>) . showText . pStatSolvedCount)

--------------------------------------------------------------------------------

-- | 'openContest' @contestId@ opens the URL to the specified contest in the
-- user's preferred web browser.
openContest :: Int -> IO ()
openContest cId =
    openBrowser ("https://codeforces.com/contest/" <> show cId) >> pure ()

--------------------------------------------------------------------------------

problemList :: ProblemOpts -> IO ()
problemList ProblemOpts {..} = problems >>= either printError printProblems
  where
    problems = fmap (filter inRatingRange) <$> getProblems []
    inRatingRange p = case problemRating p of
        Nothing -> False
        Just r  -> optMinRating <= r && r <= optMaxRating

printProblems :: [Problem] -> IO ()
printProblems ps = forM_ (makeTable headers rows) T.putStrLn
  where
    headers = [("#", 6), ("Name", 40), ("Rating", 6)]
    rows    = map
        (\Problem {..} ->
            [ plainCell $ maybe "" showText problemContestId <> problemIndex
            , plainCell problemName
            , maybe blankCell ratingCell problemRating
            ]
        )
        ps

--------------------------------------------------------------------------------

standingsList :: Int -> UserConfig -> StandingOpts -> IO ()
standingsList cId cfg opts =
    runExceptT (printStandings cId cfg opts) >>= either printError pure

printStandings
    :: Int -> UserConfig -> StandingOpts -> ExceptT ResponseError IO ()
printStandings cId cfg StandingOpts {..} = do
    friends <- ExceptT $ getFriends cfg
    let mHs = if optFriends then Just (cfgHandle cfg : friends) else Nothing

    (ss, us) <- ExceptT $ getContestStandingsWithUsers StandingsParams
        { paramContestId  = cId
        , paramFrom       = optFromIndex
        , paramRowCount   = optRowCount
        , paramRoom       = optRoom
        , paramUnofficial = optShowUnofficial
        , paramHandles    = mHs
        }

    lift $ if null us
        then if optFriends
            then putStrLn
                "Neither you nor your friends participated in this contest."
            else putStrLn "Standings empty."
        else forM_ (standingsTable ss us) T.putStrLn

standingsTable :: Standings -> M.Map Handle User -> Table
standingsTable s us = makeTable headers rows
  where
    headers = [("#", 5), ("Who", 20), ("=", totalPointsColW), ("*", 5)]
        ++ map (\p -> (problemIndex p, problemColW)) (standingsProblems s)
    rows = map
        (\RanklistRow {..} ->
            [ plainCell $ showText rrRank
                , partyCell rrParty us
                , totalPointsCell rrPoints
                , plainCell $ showText rrPenalty
                ]
                ++ map (problemResultCell scoringType) rrProblemResults
        )
        (standingsRanklist s)

    scoringType     = contestType $ standingsContest s

    -- Final score in ICPC contest is number of problems solved (single digit)
    totalPointsColW = if scoringType == ScoringICPC then 2 else 5
    -- Problem score in ICPC contest is only 2-3 chars wide (e.g. "+5", "-2")
    problemColW     = if scoringType == ScoringICPC then 3 else 5

partyCell :: Party -> M.Map Handle User -> Cell
partyCell Party {..} us = case partyMembers of
    [Member {..}] -> case M.lookup memberHandle us of
        Nothing -> plainCell $ participant memberHandle
        Just u  -> coloredCell (userColor u) (participant memberHandle)
    ms -> case partyTeamName of
        Nothing       -> plainCell $ participant $ memberList ms
        Just teamName -> plainCell $ participant teamName
  where
    participant = fmtParticipation partyParticipantType
    memberList  = T.intercalate "," . map memberHandle
    userColor   = convertRankColor . rankColor . getRank . userRating

-- | 'fmtParticipation' @participantType text@ returns the @text@ with either a
-- prefix/suffix/no changes, to indicate the type of contest participation.
fmtParticipation :: ParticipantType -> Text -> Text
fmtParticipation Virtual    t = t <> " #"
fmtParticipation Contestant t = t
fmtParticipation _          t = "* " <> t

-- | 'showPoints' @points@ returns a textual representation of the points type.
-- If @points@ is an integer (e.g. @42.0@) then the integer without decimals
-- is returned (@42@), otherwise the decimals are shown.
showPoints :: Points -> Text
showPoints x = if x == fromInteger r then showText r else showText x
    where r = round x

-- | Cell showing the total points obtained by a user in the contest.
totalPointsCell :: Points -> Cell
totalPointsCell = plainCell . showPoints

-- | Cell showing the points obtained for a problem submission.
problemResultCell :: ScoringType -> ProblemResult -> Cell
problemResultCell st pr@ProblemResult {..} = if prNotAttempted pr
    then blankCell
    else case st of
        ScoringCF -> if prPoints == 0
            then coloredCell Red $ "-" <> showText prRejectedAttemptCount
            else coloredCell Green $ showPoints prPoints
        ScoringICPC -> if prPoints == 0
            then coloredCell Blue $ "-" <> showText prRejectedAttemptCount
            else coloredCell Green $ if prRejectedAttemptCount == 0
                then "+"
                else "+" <> showText prRejectedAttemptCount
        ScoringIOI -> case prPoints of
            0   -> blankCell
            100 -> coloredCell Green "100"
            x   -> plainCell (showPoints x)

--------------------------------------------------------------------------------

userInfo :: Handle -> IO ()
userInfo h = getUser h >>= either printError printUser

printUser :: User -> IO ()
printUser u = do
    let rank = getRank (userRating u)

    putStrLn ""
    T.putStrLn $ rankColored (rankColor rank) $ T.concat
        [indent, rankName rank, " ", userHandle u]
    whenJust (sequenceA [userFirstName u, userLastName u])
        $ \ns -> T.putStrLn $ indent <> T.unwords ns

    printRatings u
    printPlace u
    putStrLn ""

printRatings :: User -> IO ()
printRatings User {..} = do
    putStrLn ""
    T.putStrLn $ T.concat
        [ indent
        , "Rating:       "
        , rankColored (rankColor (getRank userRating)) (showText userRating)
        ]
    let maxRank = getRank userRating
    T.putStrLn $ T.concat
        [ indent
        , "              (max: "
        , rankColored
            (rankColor maxRank)
            (T.concat [rankName maxRank, ", ", showText userMaxRating])
        , ")"
        ]

printPlace :: User -> IO ()
printPlace User {..} = do
    whenJust (sequenceA [userCity, userCountry]) $ \xs ->
        T.putStrLn $ indent <> "City:         " <> T.intercalate ", " xs
    whenJust userOrganization
        $ \o -> T.putStrLn $ indent <> "Organisation: " <> o

indent :: Text
indent = T.replicate 6 " "

--------------------------------------------------------------------------------

userRatings :: Handle -> IO ()
userRatings h = getUserRatingHistory h >>= either printError printRatingChanges

printRatingChanges :: [RatingChange] -> IO ()
printRatingChanges rcs = forM_ (makeTable headers rows) T.putStrLn
  where
    headers =
        [("#", 3), ("Contest", 50), ("Rank", 5), ("Change", 6), ("Rating", 6)]
    rows = reverse $ zipWith
        (\RatingChange {..} num ->
            [ plainCell $ showText num
            , plainCell rcContestName
            , plainCell $ showText rcRank
            , differenceCell (rcNewRating - rcOldRating)
            , ratingCell rcNewRating
            ]
        )
        rcs
        ([1 ..] :: [Int])

--------------------------------------------------------------------------------

userStatus :: Handle -> StatusOpts -> IO ()
userStatus h StatusOpts {..} =
    getUserStatus h optStatusFrom optStatusCount
        >>= either printError printSubmissions

printSubmissions :: [Submission] -> IO ()
printSubmissions ss = forM_ (makeTable headers rows) T.putStrLn
  where
    headers =
        [ ("When"   , 12)
        , ("Problem", 35)
        , ("Lang"   , 11)
        , ("Verdict", 35)
        , ("Time"   , 7)
        , ("Memory" , 8)
        ]
    rows = map
        (\Submission {..} ->
            [ plainCell $ fmtTime submissionTime
            , plainCell $ fmtProblem submissionProblem
            , plainCell submissionProgrammingLanguage
            , verdictCell
                submissionTestset
                submissionPassedTestCount
                submissionPoints
                submissionVerdict
            , plainCell $ fmtTimeConsumed submissionTimeConsumed
            , plainCell $ fmtMemoryConsumed submissionMemoryConsumed
            ]
        )
        ss
    fmtTime = T.pack . formatTime defaultTimeLocale "%b/%d %H:%M"
    fmtProblem p = T.concat [problemIndex p, " - ", problemName p]

fmtTimeConsumed :: Int -> Text
fmtTimeConsumed x = showText x <> " ms"

fmtMemoryConsumed :: Int -> Text
fmtMemoryConsumed x = showText (x `div` 1000) <> " KB"

--------------------------------------------------------------------------------

userFriends :: UserConfig -> IO ()
userFriends cfg = getFriends cfg >>= either printError printFriends

printFriends :: [Handle] -> IO ()
printFriends = mapM_ T.putStrLn

--------------------------------------------------------------------------------

-- | 'showText' @x@ is a 'Data.Text' version of 'show'
showText :: Show a => a -> Text
showText = T.pack . show

-- | 'colored' @color text@ wraps some text around SGR codes to display it in
-- the given color.
colored :: Color -> Text -> Text
colored c s = T.concat
    [ T.pack $ setSGRCode [SetColor Foreground Dull c]
    , s
    , T.pack $ setSGRCode [Reset]
    ]

rankColored :: R.RankColor -> Text -> Text
rankColored = colored . convertRankColor

convertRankColor :: R.RankColor -> Color
convertRankColor R.Gray   = White
convertRankColor R.Green  = Green
convertRankColor R.Cyan   = Cyan
convertRankColor R.Blue   = Blue
convertRankColor R.Violet = Magenta
convertRankColor R.Orange = Yellow
convertRankColor R.Red    = Red

-- | Prints an error in more user-friendly formatting
printError :: ResponseError -> IO ()
printError = T.putStrLn . colored Red . ("Error:\n" <>) . T.pack . show

plainCell :: Text -> Cell
plainCell = Cell [Reset]

coloredCell :: Color -> Text -> Cell
coloredCell c = Cell [SetColor Foreground Dull c]

blankCell :: Cell
blankCell = plainCell ""

ratingCell :: Int -> Cell
ratingCell x =
    let color = convertRankColor $ rankColor $ getRank x
    in coloredCell color (showText x)

-- | 'differenceCell' @diff@ colors a number red, white or green, depending on
-- whether it's negative, 0, or positive.
differenceCell :: Int -> Cell
differenceCell x
    | x > 0     = coloredCell Green $ "+" <> showText x
    | x == 0    = plainCell $ " " <> showText x
    | otherwise = coloredCell Red $ showText x

-- | 'verdictCell' @testset passedTestCount points verdict@ returns a cell
-- displaying the status of a submission, such as "Accepted" or "Wrong answer on
-- pretest 2".
verdictCell :: Testset -> Int -> Maybe Points -> Maybe Verdict -> Cell
verdictCell _       _      _      Nothing  = plainCell "In queue"
verdictCell testset passed points (Just v) = case v of
    Ok -> case testset of
        Tests      -> coloredCell Green "Accepted"
        Samples    -> coloredCell Green "Samples passed"
        Pretests   -> coloredCell Green "Pretests passed"
        Challenges -> coloredCell Green "Challenges passed"
    Partial -> coloredCell Yellow $ maybe
        "Partial result"
        (\pts -> T.concat ["Partial result: ", showText pts, " points"])
        points
    Challenged              -> coloredCell Red "Hacked"
    CompilationError        -> plainCell $ verdictText v
    Skipped                 -> plainCell $ verdictText v
    SecurityViolated        -> plainCell $ verdictText v
    Crashed                 -> plainCell $ verdictText v
    InputPreparationCrashed -> plainCell $ verdictText v
    Rejected                -> plainCell $ verdictText v
    _ ->
        let
            currTest = passed + 1
            clr      = if v == Testing then White else Blue
            text     = T.concat
                [ verdictText v
                , " on "
                , T.toLower . T.init $ showText testset
                , " "
                , showText currTest
                ]
        in coloredCell clr text

-- | Shows the verdict of a contest submission.
contestSubmissionCell :: Maybe Submission -> Cell
contestSubmissionCell Nothing                = plainCell "-"
contestSubmissionCell (Just Submission {..}) = verdictCell
    submissionTestset
    submissionPassedTestCount
    submissionPoints
    submissionVerdict

--------------------------------------------------------------------------------
