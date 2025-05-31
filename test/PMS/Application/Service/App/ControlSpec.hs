{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Application.Service.App.ControlSpec (spec) where

import Test.Hspec
import Data.Default
import Control.Lens
import qualified Control.Concurrent as C

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Application.Service.App.Control as SUT

-- |
--
data SpecContext = SpecContext {}

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  return SpecContext {}

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Spec."
  beforeAll setUpOnce $ 
    afterAll tearDownOnce . 
      beforeWith setUp . 
        after tearDown $ run

-- |
--
setUpOnce :: IO SpecContext
setUpOnce = do
  putStrLn "[INFO] EXECUTED ONLY ONCE BEFORE ALL TESTS START."
  defaultSpecContext

-- |
--
tearDownOnce :: SpecContext -> IO ()
tearDownOnce _ = do
  putStrLn "[INFO] EXECUTED ONLY ONCE AFTER ALL TESTS FINISH."

-- |
--
setUp :: SpecContext -> IO SpecContext
setUp _ = do
  putStrLn "[INFO] EXECUTED BEFORE EACH TEST STARTS."
  defaultSpecContext

-- |
--
tearDown :: SpecContext -> IO ()
tearDown _ = do
  putStrLn "[INFO] EXECUTED AFTER EACH TEST FINISHES."

-- |
--
run :: SpecWith SpecContext
run = do
  describe "runApp" $ do
    context "when AppData default" $ do
      it "should be run" $ \_ -> do 
        putStrLn "[INFO] EXECUTING THE FIRST TEST."

        let args = def
            apps = [sample1, sample2]
            
        SUT.run args apps


  where 
    sample1 :: DM.DomainContext ()
    sample1 _ = do
      putStrLn $ "[INFO] sample1 called."
      C.threadDelay (2 * 1000 * 1000) 
      putStrLn $ "[INFO] sample1 called. end."

    sample2 :: DM.DomainContext ()
    sample2 _ = do
      putStrLn $ "[INFO]   sample2 called."
      C.threadDelay (3 * 1000 * 1000) 
      putStrLn $ "[INFO]   sample2 called. end."
