{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module PMS.Application.Service.DM.Type where

import Data.Data
import Data.Default
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except

import qualified PMS.Domain.Model.DM.Type as DM


--------------------------------------------------------------------------------
-- |
--
data ArgData = ArgData {
    _yamlArgData :: Maybe FilePath
  } deriving (Data, Show, Read, Eq)

makeLenses ''ArgData
$(deriveJSON
  defaultOptions {
      fieldLabelModifier = tail . reverse . drop (length "ArgData") . reverse
    }
  ''ArgData)

instance Default ArgData where
  def = ArgData {
        _yamlArgData = Nothing
      }


--------------------------------------------------------------------------------
-- |
--
data ConfigData = ConfigData {
    _logDirConfigData :: Maybe FilePath
  , _logLevelConfigData :: LogLevel
  , _toolsDirConfigData :: FilePath
  , _promptsDirConfigData :: FilePath
  , _resourcesDirConfigData :: FilePath
  , _workDirConfigData :: Maybe FilePath
  , _promptsConfigData :: [String]
  , _invalidCharsConfigData :: [String]
  , _invalidCmdsConfigData :: [String]
  } deriving (Show, Read, Eq)

makeLenses ''ConfigData
$(deriveJSON
  defaultOptions {
      fieldLabelModifier = tail . reverse . drop (length "ConfigData") . reverse
    }
  ''ConfigData)

instance Default ConfigData where
  def = ConfigData {
        _logDirConfigData  = def
      , _logLevelConfigData = LevelDebug
      , _toolsDirConfigData = "pty-mcp-server/tools"
      , _promptsDirConfigData = "pty-mcp-server/prompts"
      , _resourcesDirConfigData = "pty-mcp-server/resources"
      , _workDirConfigData = def
      , _promptsConfigData = [
          ">>>"
        , "]#"
        , "]$"
        , ")?"
        , "Password:"
        , "password:"
        , "ghci>"
        ]
      , _invalidCharsConfigData = [
          "&&"
        , "||"
        , "|"
        , ".."
        , "reboot"
        , "shutdown"
        , "restart"
        , "kill"
        ]
      , _invalidCmdsConfigData = [
          "del", "erase", "rd", "rmdir", "format"
        , "shutdown", "restart", "taskkill"
        , "rm", "mv", "dd", "chmod", "chown"
        , "reboot", "kill", "nc", "sudo", "su"
        ]
      }


--------------------------------------------------------------------------------
-- |
--
data AppData = AppData {
               _appsAppData :: [DM.DomainContext ()]
             }
instance Default AppData where
  def = AppData {
          _appsAppData = []
        }

makeLenses ''AppData

-- |
--
type AppContext = ReaderT AppData (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))
