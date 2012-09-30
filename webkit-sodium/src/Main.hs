module Main (
    main
) where

import Engine
import Freecell

import Graphics.UI.Gtk
       (widgetShowAll, mainQuit, onDestroy, containerAdd,
        scrolledWindowNew, windowSetPosition, windowSetDefaultSize,
        windowNew, mainGUI, initGUI)
import Graphics.UI.Gtk.WebKit.WebView
       (webViewNew, webViewGetDomDocument)
import Graphics.UI.Gtk.WebKit.DOM.Document
       (documentCreateElement, documentGetElementById, documentGetBody)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement
       (htmlElementInsertAdjacentElement, htmlElementSetInnerHTML,
        htmlElementInsertAdjacentHTML)
import Graphics.UI.Gtk.WebKit.Types (castToHTMLDivElement)
import Graphics.UI.Gtk.WebKit.DOM.CSSStyleDeclaration
       (cssStyleDeclarationSetProperty)
import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad.Trans ( liftIO )
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))
import Graphics.UI.Gtk.WebKit.DOM.Element
import Graphics.UI.Gtk.WebKit.DOM.Node
import Control.Monad
import System.Random
import FRP.Sodium
import Graphics.UI.Gtk.WebKit.GHCJS (runWebGUI)
import Control.Concurrent (threadDelay, forkIO)

-- Comments show how what these FFI calls should work when the
-- code compiled is compiled with GHCJS
main = do
  runWebGUI $ \ webView -> do
    doc <- webViewGetDomDocument webView -- webView.document
    Just body <- documentGetBody doc     -- doc.body

    -- If we are in the browser let's shrink the terminal window to make room
    mbTerminal    <- fmap castToHTMLDivElement   <$> documentGetElementById doc "terminal"
    case mbTerminal of
      Just terminal -> do
        Just style <- elementGetStyle terminal
        cssStyleDeclarationSetProperty style "height" "100" ""
      _             -> return ()

    Just div <- fmap castToHTMLDivElement <$> documentCreateElement doc "div"
    elementSetAttribute div "style" "position:relative;left:0px;top:0px;background-color:#e0d0ff;width:700px;height:500px"
    nodeAppendChild body (Just div)
    unlisten <- engine doc div =<< mkFreecell

    -- Prevent finalizers running too soon
    forkIO $ forever (threadDelay 1000000000) >> unlisten

    return ()












