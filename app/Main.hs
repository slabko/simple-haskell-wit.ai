{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Network.Wit hiding (session, token)
import qualified Network.Wit as W (session, token)
import qualified Data.HashMap.Strict as M
import           Control.Monad.State.Lazy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Control.Lens
import           System.IO (hFlush, stdout)
import           Control.Exception
import qualified System.Random as R

import           Actions
import           Actions.GetForecast
--------------------------------------------------------------------------------
data AppState = AppState { 
    _context :: Value
  , _params  :: Value
  , _config  :: Config IO
} 

makeLenses ''AppState

data ExecutingException = CommandNotFound Text deriving (Show)
instance Exception ExecutingException

--------------------------------------------------------------------------------
main :: IO ()
main = do
  rnd <- R.randomIO :: IO Int
  let sess = T.pack . show $ rnd
  let emptyState = AppState Null Null (makeConfig sess)
  evalStateT (forever $ getUserMessage >>= next . Just) emptyState
  
next :: Maybe Text -> StateT AppState IO ()
next msg = do
  conf   <- use config
  cont   <- use context
  status <- liftIO $ converse conf cont msg
  process status

process :: ConverseStatus -> StateT AppState IO ()
process (Message msg)    = printMessage msg >> next Nothing
process (Action name ps) = action name ps   >> next Nothing
process Stop             = return ()

action :: Text -> Value -> StateT AppState IO ()
action name ps = do
  modify' (`mergeAppState` ps)
  s <- use params
  c <- executeAction name s
  context .= c

--------------------------------------------------------------------------------
getUserMessage :: (MonadIO m) => m Text
getUserMessage = do
  liftIO $ putStr "> " >> hFlush stdout
  liftIO T.getLine

printMessage :: (MonadIO m) => Text -> m ()
printMessage msg = liftIO $ T.putStrLn msg

executeAction :: (MonadIO m) => Text -> Value -> m Value
executeAction "getForecast" v =  execute getForcast v
executeAction cmd _ = liftIO . throwIO $ CommandNotFound cmd 

execute :: (FromValue i, ToValue o, MonadIO m) => 
           (i -> IO o) -> Value -> m Value
execute f v = do
  c <- liftIO . f $ fromValue v
  return $ toValue c

--------------------------------------------------------------------------------
makeConfig :: Text -> Config IO
makeConfig s = defaultConfig & W.token .~ "--- YOUR TOKEN HERE ---"
                             & W.session .~ s

mergeAppState :: AppState -> Value -> AppState
mergeAppState (AppState cx (Object m1) cf) (Object m2) = AppState cx (Object $ M.union m2 m1) cf
mergeAppState (AppState cx Null cf) (Object m) = AppState cx (Object m) cf 
mergeAppState (AppState cx (Object m) cf) Null = AppState cx (Object m) cf
mergeAppState state _ = state

