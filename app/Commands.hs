--------------------------------------------------------------------------------

module Commands
    ( Command(..)
    , ProblemsOpts(..)
    , parseCommands
    ) where

import Data.Text (Text)

import Options.Applicative

--------------------------------------------------------------------------------

data Command
    = ContestsCmd Bool Bool
    | ProblemsCmd ProblemsOpts
    | RatingsCmd Text
    | UserCmd Text
    deriving Eq

data ProblemsOpts = ProblemsOpts
    { optMinRating :: Int
    , optMaxRating :: Int
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
        $  command "contests" (info contestsP (progDesc ""))
        <> command "problems" (info problemsP (progDesc ""))
        <> command "ratings"  (info ratingsP (progDesc ""))
        <> command "user"     (info userP (progDesc ""))

contestsP :: Parser Command
contestsP =
    ContestsCmd
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
                <> value (fst defaultRatingRange)
                <> metavar "INT"
                )
        <*> option
                auto
                (  long "maxRating"
                <> short 'R'
                <> help "Filter problems by maximum rating."
                <> showDefault
                <> value (snd defaultRatingRange)
                <> metavar "INT"
                )
    where defaultRatingRange = (0, 3500)

ratingsP :: Parser Command
ratingsP = RatingsCmd <$> argument str (metavar "HANDLE")

userP :: Parser Command
userP = UserCmd <$> argument str (metavar "HANDLE")

--------------------------------------------------------------------------------
