--------------------------------------------------------------------------------

-- | Standings command.
module Codeforces.App.Commands.StandingsCmd
    ( standingsList
    ) where

import           Codeforces.API
import           Codeforces.App.Format
import           Codeforces.App.Options
import           Codeforces.App.Table
import           Codeforces.App.Watcher
import           Codeforces.Error

import           Control.Monad.Trans.Except

import qualified Data.Map                      as M
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

import           System.Console.ANSI.Types

--------------------------------------------------------------------------------

standingsList :: ContestId -> UserConfig -> StandingOpts -> IO ()
standingsList cId cfg StandingOpts {..} =
    handleWatch optStandWatch $ runExceptT $ do
        friends <- handleAPI $ getFriends cfg

        let mHs = if optFriends
                then Just (cfgHandle cfg : friends)
                else Nothing

        (standings, rcs) <- handleAPI $ getContestStandings' StandingsParams
            { paramContestId  = cId
            , paramFrom       = Just optFromIndex
            , paramRowCount   = Just optRowCount
            , paramRoom       = optRoom
            , paramUnofficial = optShowUnofficial
            , paramHandles    = mHs
            }

        if null (standingsRanklist standings)
            then
                throwE
                    (if optFriends
                        then StandingsWithFriendsEmpty
                        else StandingsEmpty
                    )
            else pure $ standingsTable standings rcs

standingsTable :: Standings -> M.Map Handle RatingChange -> Table
standingsTable s rcs = makeTable headers rows
  where
    headers = [("#", 5), ("Who", 20), ("=", totalPointsColW), ("*", 5)]
        ++ map (\p -> (problemIndex p, problemColW)) (standingsProblems s)
    rows = map
        (\RanklistRow {..} ->
            [ plainCell $ showText rrRank
                , partyCell rrParty rcs
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

partyCell :: Party -> M.Map Handle RatingChange -> Cell
partyCell Party {..} rcs = case partyMembers of
    [Member {..}] -> case M.lookup memberHandle rcs of
        Nothing -> plainCell $ participant $ unHandle memberHandle
        Just rc ->
            coloredCell (userColor rc) (participant $ unHandle memberHandle)

    ms -> case partyTeamName of
        Nothing       -> plainCell $ participant $ memberList ms
        Just teamName -> plainCell $ participant teamName
  where
    participant = fmtParticipation partyParticipantType
    memberList  = T.intercalate "," . map (unHandle . memberHandle)
    userColor   = convertRankColor . rankColor . getRank . rcOldRating

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
