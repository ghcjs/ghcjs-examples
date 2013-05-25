{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}
module Main (
    main
  , lazyLoad_wirePrime
  , lazyLoad_wireHamlet
) where

import Prelude hiding (catch)
import Graphics.UI.Gtk.WebKit.GHCJS (currentWindow, runWebGUI)
import Control.Applicative ((<$>))
import Control.Concurrent
       (putMVar, tryTakeMVar, takeMVar, forkIO, newEmptyMVar)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Exception (catch)
import Graphics.UI.Gtk.WebKit.DOM.Document
       (documentCreateElement, documentGetElementById, documentGetBody)
import Graphics.UI.Gtk.WebKit.Types
       (HTMLDivElement(..), HTMLTextAreaElement(..), castToHTMLDivElement,
        castToHTMLTextAreaElement, HTMLElement(..), HTMLInputElement(..),
        castToHTMLElement, castToHTMLInputElement)
import Graphics.UI.Gtk.WebKit.WebView (webViewGetDomDocument)
import Graphics.UI.Gtk.WebKit.DOM.Element
       (elementOnkeypress, elementOnkeyup, elementOnkeydown,
        elementGetStyle, elementFocus)
import Graphics.UI.Gtk.WebKit.DOM.CSSStyleDeclaration
       (cssStyleDeclarationSetProperty)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement
       (htmlElementSetInnerHTML)
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement
       (htmlInputElementGetValue)
import Data.Text.Lazy (Text, unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (defaultHamletSettings, shamlet)
import Text.Blaze.Html (Html)
import Text.Hamlet.RT (renderHamletRT, parseHamletRT, HamletException(..))
import Graphics.UI.Gtk.WebKit.DOM.HTMLTextAreaElement
       (htmlTextAreaElementGetValue)
import System.IO (stdout, hFlush)

-- Think of this as a global "onLoad" for all the pages on your website
main = runWebGUI $ \ webView -> do
  mbBrowserWindow <- currentWindow
  case mbBrowserWindow of
    Nothing -> putStrLn "Hello from Native Haskell"
    _       -> putStrLn "Hello from JavaScript land"
  hFlush stdout

  Just doc <- webViewGetDomDocument webView

  -- Try to find prime number entry
  mbPrimeIn  <- fmap castToHTMLInputElement <$> documentGetElementById doc "prime-in"
  mbPrimeOut <- fmap castToHTMLElement      <$> documentGetElementById doc "prime-out"
  case (mbPrimeIn, mbPrimeOut) of
    (Just primeIn, Just primeOut) -> lazyLoad_wirePrime primeIn primeOut
    _                             -> return ()

  -- Try to find hamlet input output
  mbHamletIn  <- fmap castToHTMLTextAreaElement <$> documentGetElementById doc "hamlet-in"
  mbHamletOut <- fmap castToHTMLDivElement      <$> documentGetElementById doc "hamlet-out"
  case (mbHamletIn, mbHamletOut) of
    (Just hamletIn, Just hamletOut) -> lazyLoad_wireHamlet hamletIn hamletOut
    _                               -> return ()

{-# NOINLINE lazyLoad_wirePrime #-}
lazyLoad_wirePrime :: HTMLInputElement -> HTMLElement -> IO ()
lazyLoad_wirePrime primeIn primeOut = do
  -- Set the input focus
  elementFocus primeIn

  -- We don't want to work on more than on prime number test at a time.
  -- So we will have a single worker thread and a queue with just one value.
  next <- newEmptyMVar
  forkIO . forever $ do
    n <- takeMVar next
    htmlElementSetInnerHTML primeOut . unpack $ validatePrime n

  -- Something to set the next work item
  let setNext = do
                  n <- htmlInputElementGetValue primeIn
                  htmlElementSetInnerHTML primeOut . unpack $ validatePrime n
                  tryTakeMVar next -- Discard existing next item
                  putMVar next n

  -- Lets wire up some events
  elementOnkeydown  primeIn (liftIO setNext)
  elementOnkeyup    primeIn (liftIO setNext)
  elementOnkeypress primeIn (liftIO setNext)
  return ()

-- Integer uses goog.math.Integer compiled to Javascript
isPrime :: Integer -> Bool
isPrime p = p > 1 && (all (\n -> p `mod` n /= 0)
                     $ takeWhile (\n -> n*n <= p) [2..])

validatePrimeMessage :: Integer -> Html
validatePrimeMessage p | isPrime p = [shamlet|$newline always
                                        <b>Yes</b>, #{p} is a prime|]
                       | otherwise = [shamlet|$newline always
                                        <b>No</b>, #{p} is not a prime|]

validatePrime :: String -> Text
validatePrime s = renderHtml $
  case reads s of
    [(n, "")] -> validatePrimeMessage n
    _         -> [shamlet|$newline always
                    <b>No</b>, that is not a number|]

{-# NOINLINE lazyLoad_wireHamlet #-}
lazyLoad_wireHamlet :: HTMLTextAreaElement -> HTMLDivElement -> IO ()
lazyLoad_wireHamlet hamletIn hamletOut = do
  -- Set the input focus
  elementFocus hamletIn

  -- We don't want to work on more than on prime number test at a time.
  -- So we will have a single worker thread and a queue with just one value.
  next <- newEmptyMVar
  forkIO . forever $ do
    n <- takeMVar next
    out <- tryHamlet n
    htmlElementSetInnerHTML hamletOut $ unpack out

  -- Something to set the next work item
  let setNext = do
                  n <- htmlTextAreaElementGetValue hamletIn
                  tryTakeMVar next -- Discard existing next item
                  putMVar next n

  -- Lets wire up some events
  elementOnkeydown  hamletIn (liftIO setNext)
  elementOnkeyup    hamletIn (liftIO setNext)
  elementOnkeypress hamletIn (liftIO setNext)
  return ()

tryHamlet :: String -> IO Text
tryHamlet h = (do
    rt   <- parseHamletRT defaultHamletSettings h
    html <- renderHamletRT rt [] (\_ _ -> "")
    return $ renderHtml html)
  `catch`
    \ (e :: HamletException) -> return $ renderHtml [shamlet|#{show e}|]
