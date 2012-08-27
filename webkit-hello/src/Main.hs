module Main (
    main
) where

import Graphics.UI.Gtk
       (widgetShowAll, mainQuit, onDestroy, containerAdd,
        scrolledWindowNew, windowSetPosition, windowSetDefaultSize,
        windowNew, mainGUI, initGUI)
import Graphics.UI.Gtk.WebKit.WebView
       (webViewNew, webViewGetDomDocument)
import Graphics.UI.Gtk.WebKit.DOM.Document
       (documentCreateElement, documentGetElementById, documentGetBody)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement
       (htmlElementInsertAdjacentElement, htmlElementSetInnerHTML)
import Graphics.UI.Gtk.WebKit.Types (castToHTMLElement)
import Control.Applicative ((<$>))
import Control.Monad.Trans ( liftIO )
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))
import Graphics.UI.Gtk.WebKit.DOM.Element (elementOnclick)
import Graphics.UI.Gtk.WebKit.DOM.EventM (mouseXY, target)

-- Comments show how what these FFI calls should work when the
-- code compiled is compiled with GHCJS
main = do
  initGUI
  webView <- mainWebView               -- window
  doc <- webViewGetDomDocument webView -- webView.document
  Just body <- documentGetBody doc     -- doc.body
  htmlElementSetInnerHTML body         -- body.setInnerHTML
    "<input type=\"button\"/ value=\"Hello\" id=\"test\">"
  Just button <- fmap castToHTMLElement <$>
    documentGetElementById doc "test"  -- doc.getElementById
  elementOnclick button $ do           -- button.onclick =
    target <- target                   -- window.event.target
    xy <- mouseXY                      -- window.event.X and window.event.Y
    liftIO $ do
      Just div <- fmap castToHTMLElement <$>
        documentCreateElement doc "div" -- doc.createElement
      -- doc.setInnerHTML
      htmlElementSetInnerHTML div ("You clicked at " ++ show xy)
      -- target.insertAdjacentElement
      htmlElementInsertAdjacentElement target "afterEnd" (Just div)
      return ()
  mainGUI

-- On browser this function just needs to return "window"
mainWebView = do
  window <- windowNew
  windowSetDefaultSize window 900 600
  windowSetPosition window WinPosCenter
  scrollWin <- scrolledWindowNew Nothing Nothing
  webView <- webViewNew
  window `containerAdd` scrollWin
  scrollWin `containerAdd` webView
  window `onDestroy` mainQuit
  widgetShowAll window
  return webView















