{-# LANGUAGE LambdaCase #-}

module PMS.Application.Service.App.Control where

import qualified Control.Exception.Safe as E
import Data.Default
import Data.Yaml
import Control.Lens
import System.Log.FastLogger
import System.IO

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

  conf <- maybe (pure def) decodeFileThrow (args^.yamlArgData)
  defDom <- DM.defaultDomainData

  hPutStrLn stderr $ "[INFO] PMS.Application.Service.App.Control.run conf." ++ show conf

  let domDat = defDom {
               DM._logDirDomainData   = conf^.logDirConfigData
             , DM._logLevelDomainData = conf^.logLevelConfigData
             , DM._toolsDirDomainData = conf^.toolsDirConfigData
             , DM._promptsDomainData = conf^.promptsConfigData
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
