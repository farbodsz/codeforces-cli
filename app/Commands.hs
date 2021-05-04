--------------------------------------------------------------------------------

module Commands
    ( Command(..)
    , ContestOpts(..)
    , ProblemsOpts(..)
    , StandingOpts(..)
    , parseCommands
    ) where

import Codeforces.User (Handle)

import Options.Applicative

--------------------------------------------------------------------------------

data Command
    = ContestsCmd ContestOpts
    | ProblemsCmd ProblemsOpts
    | RatingsCmd Handle
    | StandingsCmd Int StandingOpts
    | StatusCmd Handle
    | UserCmd Handle
    deriving Eq

data ContestOpts = ContestOpts
    { optIsGym  :: Bool
    , optIsPast :: Bool
    }
    deriving Eq

data ProblemsOpts = ProblemsOpts
    { optMinRating :: Int
    , optMaxRating :: Int
    }
    deriving Eq

data StandingOpts = StandingOpts
    { optShowUnofficial :: Bool
    , optFromIndex      :: Int
    , optRowCount       :: Int
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
        $   ProblemsOpts
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
ratingsP = RatingsCmd <$> argument str (metavar "HANDLE")

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
                       "If true than all participants (virtual, out of\
                       \ competition) are shown. Otherwise, only official\
                       \ contestants are shown."
                )
        <*> option
                auto
                (  long "from"
                <> short 'f'
                <> help "1-based index of the standings row to start from."
                <> showDefault
                <> value 1
                <> metavar "INT"
                )
        <*> option
                auto
                (  long "count"
                <> short 'n'
                <> help "Number of standing rows to return."
                <> showDefault
                <> value 30
                <> metavar "INT"
                )

statusP :: Parser Command
statusP = StatusCmd <$> argument str (metavar "HANDLE")

userP :: Parser Command
userP = UserCmd <$> argument str (metavar "HANDLE")

--------------------------------------------------------------------------------
