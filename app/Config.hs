--------------------------------------------------------------------------------

module Config
    ( loadConfig
    , setupConfig
    ) where

import Codeforces.Config

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import qualified Data.Text.IO as T

import System.Directory
import System.IO

--------------------------------------------------------------------------------

-- | 'loadConfig' returns the user configuration instance from the config file.
loadConfig :: IO UserConfig
loadConfig = do
    path      <- configPath
    hasConfig <- doesPathExist path
    mConfig   <- if hasConfig then decode <$> BL.readFile path else pure Nothing
    pure $ fromMaybe emptyConfig mConfig

-- | The user configuration file path is
-- @configPath/codeforces-cli/config.json@ where @configPath@ is the user's
-- 'XdgConfig' directory.
configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "codeforces-cli/config.json"

emptyConfig :: UserConfig
emptyConfig = UserConfig { cfgHandle = "", cfgKey = "", cfgSecret = "" }

--------------------------------------------------------------------------------

-- | Check if the user has an existing configuration file, and if so check if
-- the user wants to overwrite it. If so 'createConfig' creates a new
-- configuration file, otherwise nothing happens.
setupConfig :: IO ()
setupConfig = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout NoBuffering

    path     <- configPath
    exists   <- doesPathExist path

    continue <- if exists
        then do
            putStrLn
                $  "Warning: A configuration file already exists in "
                ++ path
            putStr "Do you want to continue? [Y/n]: "
            resp <- getLine
            pure $ resp == "Y"
        else pure True

    if not continue then pure () else createConfig path

-- | 'createConfig' is an interactive prompt tool to help users create
-- configuration files easily.
createConfig :: FilePath -> IO ()
createConfig path = do
    putStrLn ""
    putStrLn
        "You'll need to generate an API key via \
        \<https://codeforces.com/settings/api>\n"
    putStrLn "Then, complete the following details:\n"

    putStr "Codeforces handle: "
    handle <- T.getLine

    putStr "API key: "
    key <- T.getLine

    putStr "API secret: "
    secret <- T.getLine

    BL.writeFile path $ encode UserConfig
        { cfgHandle = handle
        , cfgKey    = key
        , cfgSecret = secret
        }
    putStrLn ""
    putStrLn $ "Successfully created a configuration file in " ++ path

--------------------------------------------------------------------------------
