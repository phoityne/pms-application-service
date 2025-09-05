{-# LANGUAGE LambdaCase #-}

module PMS.Application.Service.App.Control where

import qualified Control.Exception.Safe as E
import Data.Default
import Data.Yaml
import Control.Lens
import System.Log.FastLogger
import System.IO
import System.Directory
import System.FilePath

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DS.Utility as DM

import PMS.Application.Service.DM.Type
import PMS.Application.Service.DS.Utility
import PMS.Application.Service.DS.Core
import PMS.Application.Service.DM.Constant


-- |
--
run :: ArgData
    -> [DM.DomainContext ()]
    -> IO ()
run args apps = do
  hPutStrLn stderr "[INFO] PMS.Application.Service.App.Control.run called."

  conf <- loadConf args

  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run args: " ++ show args
  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run conf: " ++ show conf

  changeCWD args conf

  logDir <- case conf^.logDirConfigData of
    Nothing -> return Nothing
    Just p  -> Just <$> makeAbsolute p
  toolsDir <- makeAbsolute $ conf^.toolsDirConfigData
  promptsDir <- makeAbsolute $ conf^.promptsDirConfigData
  resourcesDir <- makeAbsolute $ conf^.resourcesDirConfigData

  defDom <- DM.defaultDomainData

  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run args: " ++ show args
  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run conf: " ++ show conf
  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run logDir: " ++ show logDir
  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run toolsDir: " ++ toolsDir
  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run promptsDir: " ++ promptsDir
  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run resourcesDir: " ++ resourcesDir

  let domDat = defDom {
               DM._logDirDomainData       = logDir
             , DM._logLevelDomainData     = conf^.logLevelConfigData
             , DM._toolsDirDomainData     = toolsDir
             , DM._promptsDirDomainData   = promptsDir
             , DM._resourcesDirDomainData = resourcesDir
             , DM._promptsDomainData      = conf^.promptsConfigData
             , DM._invalidCharsDomainData = conf^.invalidCharsConfigData
             , DM._invalidCmdsDomainData  = conf^.invalidCmdsConfigData
             }
      appDat = def {
               _appsAppData = apps
             }
  DM.createLogger domDat _LOG_FILE_NAME >>= runWithLogger domDat appDat


-- |
--
runWithLogger :: DM.DomainData -> AppData -> (TimedFastLogger, IO ()) -> IO ()
runWithLogger domDat appDat (logger, finalizeLogger) = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runApp domDat appDat logger app
    >>= \case
      Right _ -> return ()
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] PMS.Application.Service.App.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] PMS.Application.Service.App.Control.run exception occurred."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] PMS.Application.Service.App.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"

-- |
--
loadConf :: ArgData -> IO ConfigData
loadConf args = maybe (pure def) decodeFileThrow (args^.yamlArgData)


-- |
--
changeCWD :: ArgData -> ConfigData -> IO ()
changeCWD args conf = case conf^.workDirConfigData of
  Just dir -> do
    setCurrentDirectory dir
    hPutStrLn stderr $ "[INFO] CWD(config): " ++ dir

  Nothing -> case args^.yamlArgData of
    Just relPath -> do
      absPath <- takeDirectory <$> makeAbsolute relPath
      setCurrentDirectory absPath
      hPutStrLn stderr $ "[INFO] CWD(yaml path): " ++ absPath

    Nothing -> return ()

