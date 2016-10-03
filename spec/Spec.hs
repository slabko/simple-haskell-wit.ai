{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Network.Wit
import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict as M

sampleAction = Object $ M.fromList [("type", String "action")
                                   ,("action", String "getForecast")
                                   ,("entities", Null)]

sampleMessage = Object $ M.fromList [("type", String "msg")
                                   ,("msg", String "Message")]

sampleStop = Object $ M.fromList [("type", String "stop")]

testBackend :: Value -> Backend
testBackend v _ _ _ _ = return v

confWithResult :: Value -> Config
confWithResult v = defaultConfig & backend .~ testBackend v

{-# ANN main ("HLint: ignore Redundant do" :: String) #-}
main :: IO ()
main = hspec $ do
  describe "converse" $ do
    it "parses actions" $ do
      r <- converse (confWithResult sampleAction) Null (Just "What is the weather") 
      r `shouldBe` Action "getForecast" Null
    it "parses messages" $ do
      r <- converse (confWithResult sampleMessage) Null Nothing
      r `shouldBe` Message "Message"
    it "parses stop" $ do
      r <- converse (confWithResult sampleStop) Null Nothing
      r `shouldBe` Stop

