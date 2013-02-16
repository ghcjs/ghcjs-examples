{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables, Rank2Types #-}
-----------------------------------------------------------------------------
--
-- Module      :  Demo.Threading
-- Copyright   :
-- License     :  BSD3
--
-- |
--
-----------------------------------------------------------------------------

module Demo.Threading (
    counting
  , primes
  , isPrime
) where

import WebKitUtils
       (getDivElementById, getInputElementById, getImageElementById)
import Control.Monad.Trans ( liftIO )
import Graphics.UI.Gtk.WebKit.Types
       (Document(..), HTMLDivElement(..))
import Control.Concurrent
       (putMVar, tryTakeMVar, takeMVar, newEmptyMVar, threadDelay, forkIO)
import Control.Monad (forever, forM_)
import Graphics.UI.Gtk (postGUIAsync, postGUISync)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement
       (htmlElementSetInnerHTML, htmlElementSetInnerText)
import Data.Text.Lazy (Text, unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (Html, shamlet)
import Graphics.UI.Gtk.WebKit.DOM.HTMLInputElement
       (htmlInputElementGetValue)
import Graphics.UI.Gtk.WebKit.DOM.Element
       (elementSetAttribute, elementOnkeyup, elementOnkeypress,
        elementOnkeydown)

-- | Count to 10 slowly
counting :: IO ()
counting = do
    forkIO $ do
        forM_ [1..10] $ \ n -> do
            print n
            threadDelay 1000000
        putStrLn "POW!!!"
    return ()


primes :: Document -> HTMLDivElement -> IO ()
primes doc example = do
    htmlElementSetInnerHTML example . unpack $ renderHtml
        [shamlet|$newline always
            <p>
                Know any good prime numbers?
                <input #num size="8">
                <div #prime>
        |]
    numInput <- getInputElementById doc "num"
    prime    <- getDivElementById   doc "prime"

    -- Single worker thread and a queue with just one value.
    next <- newEmptyMVar
    forkIO . forever $ do
      n <- takeMVar next
      postGUISync . htmlElementSetInnerHTML prime . unpack $ validatePrime n

    -- Something to set the next work item
    let setNext = do
                    n <- htmlInputElementGetValue numInput
                    tryTakeMVar next -- Discard existing next item
                    putMVar next n

    -- Lets wire up some events
    elementOnkeydown  numInput (liftIO setNext)
    elementOnkeyup  numInput (liftIO setNext)
    elementOnkeypress numInput (liftIO setNext)
    return ()

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

