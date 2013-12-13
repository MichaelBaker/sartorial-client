{-# LANGUAGE JavaScriptFFI #-}

import GHCJS.Foreign
import GHCJS.Types

foreign import javascript unsafe
  "document.getElementById($1).innerhtml = $2"
  setHtmlById :: JSString -> JSString -> IO ()

main = do
  setHtmlById (toJSString "current-players") (toJSString "<ul><li>one</li><li>two</li></ul>")
