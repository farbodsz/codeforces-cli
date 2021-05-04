--------------------------------------------------------------------------------

module Commands
    ( Command(..)
    , ContestOpts(..)
    , ProblemOpts(..)
    , StandingOpts(..)
    , StatusOpts(..)
    , parseCommands
    ) where

import Codeforces.User (Handle)

import Options.Applicative

--------------------------------------------------------------------------------

data Command
    = ContestsCmd ContestOpts
    | ProblemsCmd ProblemOpts
    | RatingsCmd Handle
    | StandingsCmd Int StandingOpts
    | StatusCmd Handle StatusOpts
    | UserCmd Handle
    deriving Eq

data ContestOpts = ContestOpts
    { optIsGym  :: Bool
    , optIsPast :: Bool
    }
    deriving Eq

data ProblemOpts = ProblemOpts
    { optMinRating :: Int
    , optMaxRating :: Int
    }
    deriving Eq

data StandingOpts = StandingOpts
    { optShowUnofficial :: Bool
    , optFromIndex      :: Int
    , optRowCount       :: Int
    , optRoom           :: Maybe Int
    }
    deriving Eq

data StatusOpts = StatusOpts
    { optStatusFrom  :: Int
    , optStatusCount :: Int
    }
    deriving Eq

--------------------------------------------------------------------------------

parseCommands :: IO Command
parseCommands = execParser opts

opts :: ParserInfo Command
opts = info (commandP <**> helper) (header "Codeforces CLI" <> fullDesc)

commandP :: Parser Command
commandP =
    subparser
        $  command
               "contests"
               (info contestsP (progDesc "Upcoming or past contests"))
        <> command
               "problems"
               (info problemsP (progDesc "View and filter problem sets"))
        <> command
               "ratings"
               (info ratingsP (progDesc "Rating changes of a user"))
        <> command
               "standings"
               (info standingsP (progDesc "Standings table of a contest"))
        <> command
               "status"
               (info statusP (progDesc "Recent submissions of a user"))
        <> command "user" (info userP (progDesc "Information about a user"))

contestsP :: Parser Command
contestsP =
    fmap ContestsCmd
        $   ContestOpts
        <$> switch
                (  long "gym"
                <> short 'g'
                <> help
                       "If true then gym contests are returned,\
                       \ otherwise, just regular contests are shown."
                )
        <*> switch
                (long "past" <> short 'p' <> help
                    "Displays past contests instead of upcoming contests."
                )

problemsP :: Parser Command
problemsP =
    fmap ProblemsCmd
        $   ProblemOpts
        <$> option
                auto
                (  long "minRating"
                <> short 'r'
                <> help "Filter problems by minimum rating."
                <> showDefault
                <> value 0
                <> metavar "INT"
                )
        <*> option
                auto
                (  long "maxRating"
                <> short 'R'
                <> help "Filter problems by maximum rating."
                <> showDefault
                <> value 3500
                <> metavar "INT"
                )

ratingsP :: Parser Command
ratingsP = RatingsCmd <$> handleArg

standingsP :: Parser Command
standingsP =
    StandingsCmd <$> argument auto (metavar "CONTEST_ID") <*> standingOpts

standingOpts :: Parser StandingOpts
standingOpts =
    StandingOpts
        <$> switch
                (  long "unofficial"
                <> short 'u'
                <> help
                       "If true then all participants (virtual, out of\
                       \ competition) are shown. Otherwise, only official\
                       \ contestants are shown."
                )
        <*> fromOpt
        <*> countOpt
        <*> optional
                (option
                    auto
                    (  long "room"
                    <> short 'r'
                    <> help
                           "If specified, then only participants from this room\
                           \ are shown. Otherwise, all participants are shown."
                    <> metavar "INT"
                    )
                )

statusP :: Parser Command
statusP = StatusCmd <$> handleArg <*> statusOpts

statusOpts :: Parser StatusOpts
statusOpts = StatusOpts <$> fromOpt <*> countOpt

userP :: Parser Command
userP = UserCmd <$> handleArg

--------------------------------------------------------------------------------

handleArg :: Parser Handle
handleArg = argument str (metavar "HANDLE" <> help "Codeforces user handle.")

fromOpt :: Parser Int
fromOpt = option
    auto
    (  long "from"
    <> short 'f'
    <> help "1-based index of the row to start from."
    <> showDefault
    <> value 1
    <> metavar "INT"
    )

countOpt :: Parser Int
countOpt = option
    auto
    (  long "count"
    <> short 'n'
    <> help "Number of rows to return."
    <> showDefault
    <> value 40
    <> metavar "INT"
    )

--------------------------------------------------------------------------------
