{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Network.Wit
    ( Config(Config)
    , Backend
    , ConverseException(..)
    , ConverseStatus(..)
    , defaultConfig
    , converse
    , C.token
    , C.session
    , C.version
    , C.url
    , C.backend
    ) where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString (append)
import           Data.Text (Text)
import           Data.Text.Encoding
import           Data.Aeson.Lens
import           Network.HTTP.Types.Header
import           Control.Monad.Catch (MonadThrow, throwM)

import           Network.Wit.Types (Config(..), Backend, ConverseException(..))
import qualified Network.Wit.Types as C (token, session, version, url, backend)
import           Network.Wit.Backend (defaultBackend)
--------------------------------------------------------------------------------

data ConverseStatus = Stop 
                    | Message Text 
                    | Action Text Value 
                    deriving (Show, Eq)

defaultConfig :: Config IO
defaultConfig = 
  Config "" "" "20160526" "https://api.wit.ai/converse" defaultBackend

converse :: (Monad m, MonadThrow m) => Config m -> Value -> Maybe Text -> m ConverseStatus
converse conf context mtext = do
  res <- backend headers params url context
  maybe (err res) return $ toConverseStatus res
  where 
    backend = conf ^. C.backend 
    url = conf ^. C.url
    headers = headersFromConfig conf
    params = maybe id (appendParam "q") mtext $ paramsFromConfig conf
    appendParam k v ps = (k,v):ps
    err val = throwM (UnexpectedResponse val)

      
-------------------------------------------------------------------------------- 
toConverseStatus :: Value -> Maybe ConverseStatus
toConverseStatus val = 
  case val ^? key "type" of
    Just "action" -> Action <$> (val ^? key "action" . _String)
                            <*> (val ^? key "entities" . _Value)
    Just "msg"    -> Message <$> (val ^? key "msg" . _String)
    Just "stop"   -> Just Stop
    _             -> Nothing

headersFromConfig :: Config m -> [Header]
headersFromConfig conf = [(hAuthorization, "Bearer " `append` token)]
  where
    token = encodeUtf8 $ conf ^. C.token

paramsFromConfig :: Config m -> [(Text, Text)]
paramsFromConfig conf = [ ("session_id", conf ^. C.session) 
                        , ("v", conf ^. C.version)]

