{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Sample.UtilitySpec (spec) where

import Test.Hspec
import Data.Default
import qualified System.Log.Logger as L

import Spec.Sample.Constant

import Sample.Type
import qualified Sample.Utility as U


-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Spec."
  beforeAll_ setUpOnce
    . afterAll_ tearDownOnce
    . before_ setUp
    . after_  tearDown
    $ run

  where
    setUpOnce :: IO ()
    setUpOnce = do
      putStrLn "[INFO] すべての試験開始前に1回だけ実施"

    tearDownOnce ::IO ()
    tearDownOnce = do
      putStrLn "[INFO] すべての試験終了後に1回だけ実施"

    setUp :: IO ()
    setUp = do
      putStrLn "[INFO] 各試験の開始前に実施"

    tearDown :: IO ()
    tearDown = do
      putStrLn "[INFO] 各試験の終了後に実施"

-- |
--
run :: Spec
run = do
  describe "runApp" $ do
    context "when AppData default" $ do
      it "should be 10" $ do
        putStrLn "[INFO] 1件目の試験を実施"
        let expect = 10

        (Right actual) <- U.runApp def app10

        expect `shouldBe` actual

      it "should exception catched" $ do
        putStrLn "[INFO] 2件目の試験を実施"

        (Left actual) <- U.runApp def appLeft

        actual `shouldSatisfy` (not . null)
        U.runApp def appLeft `shouldReturn` Left "Prelude.head: empty list"

      it "should exception throwed" $ do
        putStrLn "[INFO] 3件目の試験を実施"

        appException `shouldThrow` anyException
        appException `shouldThrow` errorCall "Prelude.head: empty list"



-- |
--
app10 :: AppContext Int
app10 = do
  U.liftIOE $ print ("app called." :: String)
  U.liftIOE $ L.debugM  _LOG_SPEC "This is a debug log message"
  return 10


-- |
--
appLeft :: AppContext ()
appLeft = do
  U.liftIOE $ print $ head ""
  return ()


-- |
--
appException :: IO ()
appException = print $ head ""

