--------------------------------------------------------------------------------

module Commands
    ( Command(..)
    , parseCommands
    ) where

import Options.Applicative

--------------------------------------------------------------------------------

data Command = UserCmd String
    deriving Eq

--------------------------------------------------------------------------------

parseCommands :: IO Command
parseCommands = execParser opts

opts :: ParserInfo Command
opts = info (commandP <**> helper) (header "Codeforces CLI" <> fullDesc)

commandP :: Parser Command
commandP = subparser $ command "user" (info userP (progDesc ""))

userP :: Parser Command
userP = UserCmd <$> argument str (metavar "HANDLE")

--------------------------------------------------------------------------------
