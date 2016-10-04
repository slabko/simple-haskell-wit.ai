module Network.Wit.Backend 
    ( defaultBackend
    )where

import           Control.Lens
import           Network.Wreq 
import           Data.Aeson.Lens
import           Control.Exception
import           Network.Wit.Types (Backend, ConverseException(..))

defaultBackend :: Backend IO
defaultBackend hs ps url body = 
  postWith opts url body >>= getJSONPayload
  where
    opts = defaults & headers .~ hs & params .~ ps
    getJSONPayload = maybe parseErr return . parseBody
    parseBody = (^? responseBody . _Value)
    parseErr = throwIO NotJSONResponse

