module Main (
    main
) where

import           Engine
import           Freecell

import           Control.Applicative           ((<$>))
import           Control.Arrow
import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Monad
import           Control.Monad.Trans           (liftIO)
import           FRP.Sodium
import           GHCJS.DOM                     (runWebGUI,
                                                webViewGetDomDocument)
import           GHCJS.DOM.CSSStyleDeclaration (setProperty)
import           GHCJS.DOM.Document            (createElement, getBody,
                                                getElementById)
import           GHCJS.DOM.Element
import           GHCJS.DOM.HTMLElement         (insertAdjacentElement,
                                                insertAdjacentHTML)
import           GHCJS.DOM.Node
import           GHCJS.DOM.Types               (castToHTMLDivElement)
import           System.Random

-- Comments show how what these FFI calls should work when the
-- code compiled is compiled with GHCJS
main = do
  runWebGUI $ \ webView -> do
    Just doc <- webViewGetDomDocument webView -- webView.document
    Just body <- getBody doc     -- doc.body

    -- If we are in the browser let's shrink the terminal window to make room
    mbTerminal    <- fmap castToHTMLDivElement   <$> getElementById doc "terminal"
    case mbTerminal of
      Just terminal -> do
        Just style <- getStyle terminal
        setProperty style "height" (Just "100") ""
      _             -> return ()

    Just div <- fmap castToHTMLDivElement <$> createElement doc (Just "div")
    setAttribute div "style" "position:relative;left:0px;top:0px;background-color:#e0d0ff;width:700px;height:500px"
    setAttribute div "id" "freecell"
    appendChild body (Just div)
    unlisten <- engine webView "freecell" =<< mkFreecell

    -- Prevent finalizers running too soon
    forkIO $ forever (threadDelay 1000000000) >> unlisten

    return ()












