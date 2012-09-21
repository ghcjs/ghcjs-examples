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
import Graphics.UI.Gtk.WebKit.Types (castToHTMLElement)
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

-- Comments show how what these FFI calls should work when the
-- code compiled is compiled with GHCJS
main = do
  unlisten <- runWebGUI $ \ webView -> do
    doc <- webViewGetDomDocument webView -- webView.document
    Just body <- documentGetBody doc     -- doc.body
    htmlElementSetInnerHTML body $       -- body.setInnerHTML
      "Haskell Freecell"++
      "<div style=\"position:relative;left:0px;top:0px;background-color:#e0d0ff;width:700px;height:500px\" "++
      "id=\"test\" draggable=\"false\"></div>"
    Just div <- fmap castToHTMLElement <$>
      documentGetElementById doc "test"  -- doc.getElementById
    engine doc div =<< mkFreecell
  -- unlisten
  return ()












