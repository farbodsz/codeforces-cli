--------------------------------------------------------------------------------

module Commands
    ( Command(..)
    , parseCommands
    ) where

import Options.Applicative

--------------------------------------------------------------------------------

data Command = ContestsCmd Bool Bool | UserCmd String
    deriving Eq

--------------------------------------------------------------------------------

parseCommands :: IO Command
parseCommands = execParser opts

opts :: ParserInfo Command
opts = info (commandP <**> helper) (header "Codeforces CLI" <> fullDesc)

commandP :: Parser Command
commandP = subparser
    $  command "contests" (info contestsP (progDesc "")) 
    <> command "user" (info userP (progDesc ""))

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

userP :: Parser Command
userP = UserCmd <$> argument str (metavar "HANDLE")

--------------------------------------------------------------------------------
