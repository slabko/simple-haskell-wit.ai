{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import           Network.Wit hiding (session, token)
import qualified Network.Wit as W (session, token)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Control.Monad.State.Lazy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Aeson (Value(..))
import           Data.Aeson.Lens
import           Control.Lens
import           System.IO (hFlush, stdout)
import           Control.Exception
import qualified System.Random as R
import           Data.Monoid (First)
--------------------------------------------------------------------------------
data AppState = AppState { 
    _context :: HashMap Text Value
  , _params  :: HashMap Text Text
  , _config  :: Config 
} 

makeLenses ''AppState

data ExecutingException = CommandNotFound Text deriving (Show)
instance Exception ExecutingException

--------------------------------------------------------------------------------
main :: IO ()
main = do
  rnd <- R.randomIO :: IO Int
  let sess = T.pack . show $ rnd
  let emptyState = AppState M.empty M.empty (makeConfig sess)
  evalStateT (getUserMessage >>= next . Just) emptyState
  
process :: ConverseStatus -> StateT AppState IO ()
process (Action action ps) = execute action ps >> next Nothing
process (Message msg) = printMessage msg >> next Nothing
process Stop = getUserMessage >>= next . Just

next :: Maybe Text -> StateT AppState IO ()
next msg = do
  conf <- use config
  cont <- use context
  status <- liftIO $ converse conf (Object cont) msg
  process status

execute :: Text -> Value -> StateT AppState IO ()
execute "getForecast" v = getForecast v
execute cmd _ = liftIO . throwIO $ CommandNotFound cmd 

getUserMessage :: StateT AppState IO Text
getUserMessage = do
  liftIO $ putStr ">" >> hFlush stdout
  liftIO T.getLine

printMessage :: Text -> StateT AppState IO ()
printMessage msg = liftIO $ T.putStrLn msg

--------------------------------------------------------------------------------
getForecast :: Value -> StateT AppState IO ()
getForecast v = do
  mloc <- getParam "location" (v ^? locationKey)
  mdate <- getParam "date" (v ^? dateKey)
  case (mloc, mdate) of
    (Just loc, Just date) -> context .= forecast
      where forecast = M.singleton "forecast" (forecastMessage loc date)
    (Nothing, _) -> context .= M.singleton "missingLocation" (Bool True)
    _ -> context .= M.singleton "missingDate" (Bool True)

forecastMessage :: Text -> Text -> Value
forecastMessage l d = 
  String $ mconcat ["The weather at ", d, " in ", l, " will be good."]

locationKey :: Getting (First Text) Value Text
locationKey = key "location" . values . key "value" . _String

dateKey :: Getting (First Text) Value Text
dateKey = key "datetime" . values . key "value" . _String

--------------------------------------------------------------------------------
makeConfig :: Text -> Config
makeConfig s = defaultConfig & W.token .~ "--- YOUR TOKEN HERE ---"
                             & W.session .~ s

getParam :: Text -> Maybe Text -> StateT AppState IO (Maybe Text)
getParam k Nothing = use $ params . at k
getParam k (Just val) = do
  params %= M.insert k val
  getParam k Nothing

