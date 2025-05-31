{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Application.Service.DS.Core where

import Control.Monad.Logger
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Lens
import Control.Exception
import System.IO
import Control.Monad.Except

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Application.Service.DM.Type
import PMS.Application.Service.DS.Utility

import Control.Concurrent.Async

-- |
--
app :: AppContext ()
app = flip catchError errHdl $ do
  $logDebugS DM._LOGTAG "app called."

  liftIOE $ hSetBuffering stderr LineBuffering

  domCtxs <- view appsAppData <$> ask
  domDat  <- lift ask

  liftIOE (start domDat domCtxs) >>= \case
    (_, Right _) -> $logInfoS DM._LOGTAG "some threads stopped. exit."
    (_, Left e)  -> $logErrorS DM._LOGTAG  $ T.pack $ "some threads stopped. " ++ show e

  where
    errHdl :: String -> AppContext ()
    errHdl msg = do
      $logErrorS DM._LOGTAG $ T.pack $ "app: exception occurred. skip. " ++ msg
      
    start :: DM.DomainData
          -> [DM.DomainContext ()]
          -> IO (Async (), Either SomeException ())
    start domDat domCtxs = do
      as <- mapM (go domDat) domCtxs
      waitAnyCatchCancel as

    go :: DM.DomainData -> DM.DomainContext () -> IO (Async ())
    go domDat domCtx = async $ domCtx domDat
