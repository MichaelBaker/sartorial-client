{-# LANGUAGE JavaScriptFFI, OverloadedStrings #-}

import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Types
import Control.Concurrent          (threadDelay)
import Data.Text                   (pack, unpack)
import Data.Default
import JavaScript.JQuery           (click, select)
import Protocol                    (Request (..), Response (..))
import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, writeTVar, readTVar, readTVarIO)
import Data.List                   (isPrefixOf)
import Safe                        (readMay)

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
  "jQuery($1).val('')"
  clearValue :: JSString -> IO JSString

foreign import javascript unsafe
  "jQuery($1).html($2)"
  setHtml :: JSString -> JSString -> IO ()

foreign import javascript unsafe
 "jQuery($1).hide()"
 hideElement :: JSString -> IO ()

foreign import javascript unsafe
 "jQuery($1).show()"
 showElement :: JSString -> IO ()

main = do
  playerName <- newTVarIO ""
  messages   <- newTVarIO []
  setup playerName
  loop  playerName messages

loop playerName messages = do
  updatePlayerList
  updateChat playerName messages
  threadDelay 1000000
  loop playerName messages

updateChat playerName messages = do
  name <- readTVarIO playerName
  if null name
    then return ()
    else do
      result <- sendCommand $ ChatUpdatesRequest name
      case result of
        ChatUpdatesResponse newMessages -> do
          messages <- atomically $ do
            existingMessages <- readTVar messages
            let finalMessages = take 10 $ existingMessages ++ newMessages
            writeTVar messages finalMessages
            return finalMessages
          setHtml "#chat-messages" $ toJSString $ concatMap (wrapLi) messages
	_ -> return ()

setup playerName = do
  playerSubmitButton <- select "#add-player-submit"
  click (addPlayer playerName) def playerSubmitButton

  commandSubmitButton <- select "#command-submit"
  click (submitCommand playerName) def commandSubmitButton
  return ()

addPlayer playerName _ = do
  hideElement "#add-player"
  name <- getValue "#add-player-input"
  atomically  $ writeTVar playerName (fromJSString name)
  showElement "#command"
  sendCommand $ AddPlayerRequest $ fromJSString name
  return ()

submitCommand playerName _ = do
  currentCommand <- getValue "#command-input"
  clearValue "#command-input"
  processCommand playerName $ fromJSString currentCommand
  return ()

processCommand playerName commandString | isPrefixOf "say " commandString = say playerName commandString
                                        | otherwise = return ()

say playerName commandString = do
  name <- readTVarIO playerName
  sendCommand $ ChatMessageRequest name $ dropWhile (== ' ') $ drop 3 commandString
  return ()

sendCommand :: Request -> IO Response
sendCommand command = do
  result <- sendAjax $ toJSString $ show command
  let responseText = fromJSString result
  case readMay responseText of
    Just a  -> return a
    Nothing -> print ("Error response " ++ responseText ++ " for command " ++ show command) >> return (ErrorResponse responseText)

updatePlayerList = do
  result  <- sendCommand PlayerListRequest
  case result of
    (PlayerListResponse names) -> do
      setHtml "#current-players" $ toJSString $ concatMap (wrapLi) names
      return ()
    _ -> putStrLn $ "Got an invalid response of " ++ show result ++ " to command PlayerListRequest"

wrapLi x = "<li>" ++ x ++ "</li>"
