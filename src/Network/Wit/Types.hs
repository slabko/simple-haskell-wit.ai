{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Wit.Types
    ( Config(Config)
    , Backend
    , ConverseException(..)
    , token
    , session
    , version
    , url
    , backend
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.Text (Text)
import           Network.HTTP.Types.Header
import           Control.Exception

type Backend = [Header] ->            -- Headers
               [(Text, Text)] ->      -- Params 
               String ->              -- URL
               Value ->               -- Payload 
               IO Value

data ConverseException = NotJSONResponse | UnexpectedResponse Value deriving (Show, Eq)
instance Exception ConverseException

data Config = Config {
    _token   :: Text
  , _session :: Text
  , _version :: Text
  , _url     :: String
  , _backend :: Backend
}

makeLenses ''Config

