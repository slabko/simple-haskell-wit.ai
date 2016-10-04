{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Network.Wit
import           Control.Lens
import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Control.Exception (SomeException, fromException)

sampleAction = Object $ M.fromList [("type", String "action")
                                   ,("action", String "getForecast")
                                   ,("entities", Null)]

sampleMessage = Object $ M.fromList [("type", String "msg")
                                   ,("msg", String "Message")]

sampleStop = Object $ M.fromList [("type", String "stop")]

sampleWronResponse = Object $ M.fromList [("type", String "action")]

testBackend :: Value -> Backend (Either SomeException)
testBackend v _ _ _ _ = return v

confWithResult :: Value -> Config (Either SomeException)
confWithResult v = defaultConfig & backend .~ testBackend v

{-# ANN main ("HLint: ignore Redundant do" :: String) #-}
main :: IO ()
main = hspec $ do
  describe "converse" $ do
    it "parses actions" $ do
      let (Right r) = converse (confWithResult sampleAction) Null (Just "What is the weather") 
      r `shouldBe` Action "getForecast" Null
    it "parses messages" $ do
      let (Right r) = converse (confWithResult sampleMessage) Null Nothing
      r `shouldBe` Message "Message"
    it "parses stop" $ do
      let (Right r) = converse (confWithResult sampleStop) Null Nothing
      r `shouldBe` Stop
    it "throws exception for unexpected response" $ do
      let (Left r) = converse (confWithResult sampleWronResponse) Null Nothing
      fromException r `shouldBe` Just (UnexpectedResponse sampleWronResponse)

