--------------------------------------------------------------------------------

module Commands
    ( Command(..)
    , ContestOpts(..)
    , InfoOpts(..)
    , ProblemOpts(..)
    , StandingOpts(..)
    , StatusOpts(..)
    , parseCommands
    ) where

import Codeforces.Types

import Options.Applicative

--------------------------------------------------------------------------------

data Command
    = ContestsCmd ContestOpts
    | FriendsCmd
    | InfoCmd Int InfoOpts
    | OpenCmd Int
    | ProblemsCmd ProblemOpts
    | RatingsCmd Handle
    | SetupCmd
    | StandingsCmd Int StandingOpts
    | StatusCmd Handle StatusOpts
    | UserCmd Handle
    | VirtualCmd Int Handle Points Int
    deriving Eq

data ContestOpts = ContestOpts
    { optIsGym  :: Bool
    , optIsPast :: Bool
    }
    deriving Eq

data InfoOpts = InfoOpts
    { optHandle :: Maybe Handle
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
    , optFriends        :: Bool
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
    hsubparser
        $  command
               "contests"
               (info contestsP (progDesc "Upcoming or past contests"))
        <> command
               "info"
               (info
                   contestInfoP
                   (progDesc
                       "Show the problems and your problem results of a contest"
                   )
               )
        <> command
               "friends"
               (info
                   friendsP
                   (progDesc "List your friends (must be authenticated)")
               )
        <> command
               "open"
               (info openP (progDesc "Open a contest in the browser"))
        <> command
               "problems"
               (info problemsP (progDesc "View and filter problem sets"))
        <> command
               "ratings"
               (info ratingsP (progDesc "Rating changes of a user"))
        <> command
               "setup"
               (info setupP (progDesc "Setup your configuration file"))
        <> command
               "standings"
               (info standingsP (progDesc "Standings table of a contest"))
        <> command
               "status"
               (info statusP (progDesc "Recent submissions of a user"))
        <> command "user" (info userP (progDesc "Information about a user"))
        <> command
               "virtual"
               (info
                   virtualP
                   (progDesc
                       "Calculate your rating after a virtual contest,\
                           \ to find what it would be if you competed live"
                   )
               )

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

contestInfoP :: Parser Command
contestInfoP = InfoCmd <$> contestIdArg <*> contestInfoOpts

contestInfoOpts :: Parser InfoOpts
contestInfoOpts = InfoOpts <$> optional
    (strOption
        (  long "user"
        <> short 'u'
        <> help
               "Codeforces user handle. If specified, it shows the contest details\
            \ of another user. If not specified, your contest details will be\
            \ shown."
        <> metavar "HANDLE"
        )
    )

friendsP :: Parser Command
friendsP = pure FriendsCmd

openP :: Parser Command
openP = OpenCmd <$> contestIdArg

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

setupP :: Parser Command
setupP = pure SetupCmd

standingsP :: Parser Command
standingsP = StandingsCmd <$> contestIdArg <*> standingOpts

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
        <*> switch
                (  long "friends"
                <> short 'f'
                <> help
                       "If true then only you and your friends will be shown\
                           \ in the standings."
                )

statusP :: Parser Command
statusP = StatusCmd <$> handleArg <*> statusOpts

statusOpts :: Parser StatusOpts
statusOpts = StatusOpts <$> fromOpt <*> countOpt

userP :: Parser Command
userP = UserCmd <$> handleArg

virtualP :: Parser Command
virtualP =
    VirtualCmd
        <$> contestIdArg
        <*> handleArg
        <*> argument auto (metavar "POINTS" <> help "Total points")
        <*> argument auto (metavar "PENALTY" <> help "Total penalty")

--------------------------------------------------------------------------------

handleArg :: Parser Handle
handleArg = argument str (metavar "HANDLE" <> help "Codeforces user handle.")

contestIdArg :: Parser Int
contestIdArg = argument auto (metavar "CONTEST_ID" <> help "ID of the contest")

fromOpt :: Parser Int
fromOpt = option
    auto
    (  long "from"
    <> short 'i'
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
