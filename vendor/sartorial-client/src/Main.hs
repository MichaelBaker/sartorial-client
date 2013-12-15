{-# LANGUAGE JavaScriptFFI #-}

import qualified GHCJS.Foreign as F
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign (ToJSString(..), FromJSString(..), newObj, toJSBool, jsNull, jsFalse, jsTrue, mvarRef)
import Control.Concurrent (threadDelay)
import JavaScript.JQuery
import JavaScript.JQuery.Internal
import Data.Text (Text, pack, unpack)
import Control.Applicative
import Data.Maybe
import Control.Monad
import Data.Default
import Protocol           (Request (..), Response (..))

main = do
  setup
  loop

setup = do
  playerNameInput <- getElement "#add-player-input"
  submitButton    <- getElement "#add-player-submit"
  click (handleNewPlayerName playerNameInput) def submitButton

handleNewPlayerName element _ = do
  name <- getVal element
  sendCommand $ AddPlayerRequest (unpack name)
  return ()

getElement = select . pack 

loop = do
  updatePlayerList
  threadDelay 1000000
  loop

sendCommand :: Request -> IO AjaxResult
sendCommand command = do
  os        <- toJSRef (def { asMethod = POST } :: AjaxSettings)
  jsCommand <- toJSRef $ show command
  F.setProp (pack "data") jsCommand os
  arr <- jq_ajax (toJSString "command") os
  dat <- F.getProp (pack "data") arr
  let d = if isNull dat then Nothing else Just (fromJSString dat)
  status <- fromMaybe 0 <$> (fromJSRef =<< F.getProp (pack "status") arr)
  return (AjaxResult status d)

updatePlayerList = do
  result  <- sendCommand PlayerListRequest
  case arData result of
    Nothing       -> return ()
    Just response -> do
      case read $ unpack response of
        (PlayerListResponse names) -> do
          let nameList = concat $ map wrapLi names
          element <- select $ pack "#current-players"
          setHtml (pack nameList) element
          return ()
        _ -> putStrLn "Got an invalid response to command PlayerListRequest"

wrapLi x = "<li>" ++ x ++ "</li>"
