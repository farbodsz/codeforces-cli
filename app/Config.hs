--------------------------------------------------------------------------------

module Config
    ( loadConfig
    ) where

import Codeforces.Config

import Data.Aeson
import Data.ByteString.Lazy as BL
import Data.Maybe

import System.Directory

--------------------------------------------------------------------------------

-- | 'loadConfig' returns the user configuration instance from the config file.
loadConfig :: IO UserConfig
loadConfig = do
    path      <- configPath
    hasConfig <- doesPathExist path
    mConfig   <- if hasConfig then decode <$> BL.readFile path else pure Nothing
    pure $ fromMaybe emptyConfig mConfig

-- | The user configuration file path is:
-- @
-- configPath/codeforces-cli/config.json
-- @
-- Where @configPath@ is the user's 'XdgConfig' directory.
--
configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "codeforces-cli/config.json"

emptyConfig :: UserConfig
emptyConfig = UserConfig { cfgHandle = "", cfgKey = "", cfgSecret = "" }

--------------------------------------------------------------------------------
