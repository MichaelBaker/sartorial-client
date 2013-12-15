{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Types
import Control.Concurrent (threadDelay)
import Data.Text          (pack, unpack)
import Data.Default
import JavaScript.JQuery  (click, select)
import Protocol           (Request (..), Response (..))

foreign import javascript interruptible
  "jQuery.ajax('/command', {type: 'POST', contentType: 'text/plain; charset=UTF-8', data: $1}).always(function (data) { $c(data) });"
  sendAjax :: JSString -> IO JSString

foreign import javascript safe
  "jQuery('#add-player-submit').click(function () { h$run(h$addPlayer($('#add-player-input').val())) })"
  addPlayerCallback :: IO ()

foreign import javascript unsafe
  "jQuery($1).val()"
  getValue :: JSString -> IO JSString

foreign import javascript unsafe
  "jQuery($1).html($2)"
  setHtml :: JSString -> JSString -> IO ()

main = do
  putStrLn "Starting Haskell"
  setup
  loop

setup = do
  playerSubmitButton <- select "#add-player-submit"
  click addPlayer def playerSubmitButton
  return ()

addPlayer _ = do
  name <- getValue "#add-player-input"
  sendCommand $ AddPlayerRequest $ fromJSString name
  return ()

loop = do
  updatePlayerList
  threadDelay 1000000
  loop

sendCommand :: Request -> IO Response
sendCommand command = do
  result <- sendAjax $ toJSString $ show command
  let response = fromJSString result
  putStrLn response
  return $ read response

updatePlayerList = do
  result  <- sendCommand PlayerListRequest
  case result of
    (PlayerListResponse names) -> do
      setHtml "#current-players" $ toJSString $ concatMap (wrapLi) names
      return ()
    _ -> putStrLn $ "Got an invalid response of " ++ show result ++ " to command PlayerListRequest"

wrapLi x = "<li>" ++ x ++ "</li>"
