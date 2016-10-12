{-# LANGUAGE OverloadedStrings #-}

module Actions.GetForecast 
  ( getForcast
  , FromValue(..)
  ) where

import           Control.Lens
import           Data.Aeson.Lens
import           Data.Text
import qualified Data.HashMap.Strict as M

import           Actions

data GetForcastInput = GetForcastInput {
    _location :: Maybe Text
  , _date     :: Maybe Text
}

data GetForcastOutput = MissingLocation | MissingDate | Forecast Text

instance FromValue GetForcastInput where
  fromValue v = GetForcastInput 
                  (v ^? key "location" . values . key "value" . _String)
                  (v ^? key "datetime" . values . key "value" . _String)


instance ToValue GetForcastOutput where
  toValue MissingLocation = Object $ M.singleton "missingLocation" (Bool True)
  toValue MissingDate     = Object $ M.singleton "missingDate" (Bool True)
  toValue (Forecast txt)  = Object $ M.singleton "forecast" (String txt)


getForcast :: GetForcastInput -> IO GetForcastOutput
getForcast (GetForcastInput Nothing _) = return MissingLocation
getForcast (GetForcastInput _ Nothing) = return MissingDate
getForcast (GetForcastInput (Just loc) (Just dt)) = 
  return . Forecast . mconcat $ ws
  where 
    ws = ["The weather at ", dt, " in ", loc, " will be good."]
