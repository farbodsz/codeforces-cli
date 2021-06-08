--------------------------------------------------------------------------------

-- | User-related commands.
module Commands.UserCmds
    ( userInfo
    , userRatings
    , userStatus
    , userFriends
    ) where

import Codeforces

import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time

import Error
import Format
import Options
import Table
import Watcher

--------------------------------------------------------------------------------

userInfo :: Handle -> IO ()
userInfo h = handleE $ runExceptT $ do
    u <- ExceptT $ getUser h

    let rank = getRank (userRating u)

    lift $ do
        putStrLn ""
        T.putStrLn $ rankColored (rankColor rank) $ T.concat
            [indent, rankName rank, " ", unHandle $ userHandle u]
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

--------------------------------------------------------------------------------

userRatings :: Handle -> IO ()
userRatings h = handleE $ runExceptT $ do
    rcs <- ExceptT $ getUserRatingHistory h

    let headers =
            [ ("#"      , 3)
            , ("Contest", 50)
            , ("Rank"   , 5)
            , ("Change" , 6)
            , ("Rating" , 6)
            ]
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

    lift $ mapM_ T.putStrLn $ makeTable headers rows

--------------------------------------------------------------------------------

userStatus :: Handle -> StatusOpts -> IO ()
userStatus h opts = handleWatch (optStatusWatch opts) (userStatusTable h opts)

userStatusTable :: Handle -> StatusOpts -> IO (Either ResponseError Table)
userStatusTable h StatusOpts {..} = runExceptT $ do
    ss <- ExceptT $ getUserStatus h optStatusFrom optStatusCount

    let headers =
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

    pure $ makeTable headers rows

fmtTime :: UTCTime -> Text
fmtTime = T.pack . formatTime defaultTimeLocale "%b/%d %H:%M"

fmtProblem :: Problem -> Text
fmtProblem p = T.concat [problemIndex p, " - ", problemName p]

--------------------------------------------------------------------------------

userFriends :: UserConfig -> IO ()
userFriends cfg = handleE $ runExceptT $ do
    fs <- ExceptT $ getFriends cfg
    lift $ mapM_ (T.putStrLn . unHandle) fs

--------------------------------------------------------------------------------
