--------------------------------------------------------------------------------

module Main where

import Codeforces.Contest
import Codeforces.Party
import Codeforces.Problem
import Codeforces.Rank hiding (RankColor(..))
import qualified Codeforces.Rank as R
import Codeforces.RatingChange
import Codeforces.Standings
import Codeforces.Submission
import Codeforces.User

import Commands

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import System.Console.ANSI

import Table

--------------------------------------------------------------------------------

main :: IO ()
main = do
    command <- parseCommands
    case command of
        ContestsCmd opts      -> contestList opts
        ProblemsCmd opts      -> problemList opts
        StandingsCmd cId opts -> standingsList cId opts
        UserCmd    h          -> userInfo h
        RatingsCmd h          -> userRatings h
        StatusCmd  h          -> userStatus h

--------------------------------------------------------------------------------

contestList :: ContestOpts -> IO ()
contestList ContestOpts {..} = do
    contests <- getContests optIsGym
    now      <- getCurrentTime
    either print (printContests . filterContests optIsPast now) contests

-- | `filterContests` @onlyPast currentTime@ filters and orders a list of
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
                <$> [ T.pack $ show contestId
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

problemList :: ProblemsOpts -> IO ()
problemList ProblemsOpts {..} = problems >>= either print printProblems
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
            [ plainCell
                $  maybe "" (T.pack . show) problemContestId
                <> problemIndex
            , plainCell problemName
            , maybe blankCell ratingCell problemRating
            ]
        )
        ps

--------------------------------------------------------------------------------

standingsList :: Int -> StandingOpts -> IO ()
standingsList cId unofficial =
    runExceptT (printStandings cId unofficial) >>= either print pure

printStandings :: Int -> StandingOpts -> ExceptT String IO ()
printStandings cId StandingOpts {..} = do
    ss <- ExceptT $ getContestStandings cId 1 40 optShowUnofficial
    let rl = standingsRanklist ss
    us <- standingsUsers rl
    lift $ forM_ (standingsTable ss us) T.putStrLn

-- | 'standingsUsers' @rows@ returns a map of @User@s included in the standings.
standingsUsers :: [RanklistRow] -> ExceptT String IO (M.Map Handle User)
standingsUsers rrs = do
    us <- ExceptT $ getUsers handles
    pure $ M.fromList $ zip handles us
  where
    handles :: [Handle]
    handles = concatMap (map memberHandle . partyMembers . rrParty) rrs

standingsTable :: Standings -> M.Map Handle User -> Table
standingsTable s us = makeTable headers rows
  where
    headers = [("#", 10), ("Who", 25), ("=", totalPointsColW), ("*", 5)]
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
    totalPointsColW = if scoringType == ScoringICPC then 2 else 6
    -- Problem score in ICPC contest is only 2-3 chars wide (e.g. "+5", "-2")
    problemColW     = if scoringType == ScoringICPC then 3 else 5

partyCell :: Party -> M.Map Handle User -> Cell
partyCell Party {..} us = case partyMembers of
    [m] -> case M.lookup (memberHandle m) us of
        Nothing -> plainCell $ memberHandle m
        Just u ->
            let
                rank = getRank (userRating u)
                rc   = convertRankColor (rankColor rank)
            in coloredCell rc (memberHandle m)
    ms -> case partyTeamName of
        Nothing       -> plainCell $ T.intercalate "," $ map memberHandle ms
        Just teamName -> plainCell teamName

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
userInfo h = getUser h >>= either print printUser

printUser :: User -> IO ()
printUser u = do
    let rank = getRank (userRating u)

    putStrLn ""
    T.putStrLn $ rankColored (rankColor rank) $ T.concat
        [indent, rankName rank, " ", userHandle u, "\n"]
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
        , rankColored
            (rankColor (getRank userRating))
            (T.pack $ show userRating)
        ]
    let maxRank = getRank userRating
    T.putStrLn $ T.concat
        [ indent
        , "              (max: "
        , rankColored
            (rankColor maxRank)
            (T.concat [rankName maxRank, ", ", T.pack $ show userMaxRating])
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
userRatings h = getUserRatingHistory h >>= either print printRatingChanges

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

userStatus :: Handle -> IO ()
userStatus h = getUserStatus h 40 >>= either print printSubmissions

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
            , plainCell $ showText submissionTimeConsumed <> " ms"
            , plainCell $ fmtMemory submissionMemoryConsumed
            ]
        )
        ss
    fmtTime = T.pack . formatTime defaultTimeLocale "%b/%d %H:%M"
    fmtProblem Problem {..} = T.concat [problemIndex, " - ", problemName]
    fmtMemory x = showText (x `div` 1000) <> " KB"

-- | `verdictCell` @testset passedTestCount points verdict@ returns a cell
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

--------------------------------------------------------------------------------

showText :: Show a => a -> Text
showText = T.pack . show

-- | `rankColored` @rankColor text@ wraps some text around SGR codes to display
-- them in the given rank color.
rankColored :: R.RankColor -> Text -> Text
rankColored c s = T.concat
    [ T.pack $ setSGRCode [SetColor Foreground Dull (convertRankColor c)]
    , s
    , T.pack $ setSGRCode [Reset]
    ]

convertRankColor :: R.RankColor -> Color
convertRankColor R.Gray   = White
convertRankColor R.Green  = Green
convertRankColor R.Cyan   = Cyan
convertRankColor R.Blue   = Blue
convertRankColor R.Violet = Magenta
convertRankColor R.Orange = Yellow
convertRankColor R.Red    = Red

plainCell :: Text -> Cell
plainCell = Cell [Reset]

coloredCell :: Color -> Text -> Cell
coloredCell c = Cell [SetColor Foreground Dull c]

blankCell :: Cell
blankCell = plainCell ""

ratingCell :: Int -> Cell
ratingCell x =
    let color = convertRankColor $ rankColor $ getRank x
    in coloredCell color $ T.pack $ show x

-- | `differenceCell` @diff@ colors a number red, white or green, depending on
-- whether it's negative, 0, or positive.
differenceCell :: Int -> Cell
differenceCell x
    | x > 0     = coloredCell Green $ T.concat ["+", T.pack $ show x]
    | x == 0    = plainCell $ T.concat [" ", T.pack $ show x]
    | otherwise = coloredCell Red $ T.pack $ show x

--------------------------------------------------------------------------------
