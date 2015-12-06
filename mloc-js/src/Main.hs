{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  BSD
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Prelude hiding((!!))
import Control.Applicative ((<$>))
import GHCJS.DOM (runWebGUI)
-- import Graphics.UI.Gtk (postGUISync, postGUIAsync)
import GHCJS.DOM
       (webViewGetDomDocument)
import GHCJS.DOM.Document
       (getElementById, createElement, getBody)
import GHCJS.DOM (WebView(..))
import GHCJS.DOM.Types
       (Document(..), HTMLDivElement(..), castToHTMLElement,
        castToHTMLInputElement, castToHTMLDivElement)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.HTMLElement (setInnerText)
import Data.Text.Lazy (Text, unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (Html, shamlet)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Element (click, focus, keyPress, keyUp, keyDown, getStyle)
import GHCJS.DOM.CSSStyleDeclaration (setProperty)
import GHCJS.DOM.Node (appendChild, insertBefore)
import Control.Monad.Reader (ReaderT(..))
import Language.Javascript.JSaddle
       (call, JSNull(..), val, array, eval, new, fun,
        valToText, valToNumber, deRefVal, (<#), (#), (!!), js, jsg)
import Control.Lens ((^.))
import Control.Monad.Trans (liftIO)
import Control.Concurrent
       (putMVar, tryTakeMVar, takeMVar, newEmptyMVar, threadDelay, forkIO)
import Control.Monad (when, forever)
import GHCJS.DOM.HTMLInputElement (getValue)
import System.IO (hPutStrLn, stdout, hFlush, stderr)
import Language.Javascript.JSaddle.Value (JSValue(..))
import qualified Data.Text as T (pack)
import Language.Javascript.JMacro (jLam, ToJExpr(..), JStat(..))
import GHCJS.DOM.EventM (mouseShiftKey)
import Engine (engine)
import Freecell (mkFreecell)
import Language.Haskell.TH (pprint, runQ)
import Language.Haskell.HsColour.CSS (hscolour)
import WebKitUtils
import Demo.DOM
import Demo.Threading
import Demo.JavaScriptFFI
import Demo.Life
import Demo.LazyLoading

mainPage :: WebView -> Document -> HTMLDivElement -> IO ()
mainPage webView doc div = do
    -- Lets use some Hamlet to replace HTerm with some HTML
    setInnerHTML div . Just . unpack $ renderHtml [shamlet|$newline always
        <link href="//netdna.bootstrapcdn.com/twitter-bootstrap/2.3.0/css/bootstrap-combined.min.css" rel="stylesheet">
        <h1 #heading>MLOC.JS GHCJS Examples
        <div .row>
            <div .span2 #menu>
                <ul>
                    <li #dom><a style="cursor: pointer">DOM Interface</a>
                    <li>Threading
                        <ul>
                            <li #counting><a style="cursor: pointer">Count to 10</a>
                            <li #primes><a style="cursor: pointer">Check Primes</a>
                    <li>JavaScript FFI (JSC)
                        <ul>
                            <li #canvasDemo><a style="cursor: pointer">Using Canvas</a>
                            <li #callHaskell><a style="cursor: pointer">Call Haskell</a>
                            <li #life><a style="cursor: pointer">Life</a>
                    <li #lazyload><a style="cursor: pointer">Lazy Loading</a>
            <div .span10 #example>
    |]
    example <- getDivElementById doc "example"
    menuItem "dom"         $ helloDOM doc
    menuItem "counting"    $ counting
    menuItem "primes"      $ primes doc example
    menuItem "canvasDemo"  $ canvasDemo webView doc example
    menuItem "callHaskell" $ callHaskell webView
    menuItem "life"        $ life webView doc example
    menuItem "lazyload"    $ lazyLoad_freecell webView doc example
    return ()
  where
    menuItem name f = do
        item <- getHTMLElementById doc name
        on item click (liftIO $ f)

main :: IO ()
main = do
  -- Running a GUI creates a WebKitGtk window in native code,
  -- but just returns the browser window when compiled to JavaScript
  runWebGUI $ \ webView -> do
    -- WebKitGtk provides the normal W3C DOM functions
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc

    -- Create a div to put our page content in
    Just div <- fmap castToHTMLDivElement <$> createElement doc (Just "div")

    -- Now we need to add this div to the document body
    -- If we are in the browser then let's shrink the terminal window to make room
    mbTerminal <- fmap castToHTMLDivElement <$> getElementById doc "terminal"
    case mbTerminal of
      Just terminal -> do
        Just style <- getStyle terminal
        setProperty style "height" (Just "200px") ""
        setProperty style "position" (Just "absolute") ""
        setProperty style "bottom" (Just "0") ""
        insertBefore body (Just div) (Just terminal)
      _ -> do
        appendChild body (Just div)

    -- Load the main page
    mainPage webView doc div
