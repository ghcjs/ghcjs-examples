{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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

import           Control.Concurrent            (forkIO, newEmptyMVar, putMVar,
                                                takeMVar, threadDelay,
                                                tryTakeMVar)
import           Control.Monad                 (forM_, forever)
import           Control.Monad.Trans           (liftIO)
import           Data.Maybe
import           Data.Text.Lazy                (Text, unpack)
import           GHCJS.DOM                     (postGUISync)
import           GHCJS.DOM.Element             (keyDown, keyPress, keyUp,
                                                setAttribute, setInnerHTML)
import           GHCJS.DOM.EventM              (on)
import           GHCJS.DOM.HTMLElement         (setInnerText)
import           GHCJS.DOM.HTMLInputElement    (getValue)
import           GHCJS.DOM.Types               (Document (..),
                                                HTMLDivElement (..))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (Html, shamlet)
import           WebKitUtils                   (getDivElementById,
                                                getImageElementById,
                                                getInputElementById)

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
    setInnerHTML example . Just . unpack $ renderHtml
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
      postGUISync . setInnerHTML prime . Just . unpack $ validatePrime n

    -- Something to set the next work item
    let setNext = do
                    n <- getValue numInput
                    tryTakeMVar next -- Discard existing next item
                    putMVar next (fromMaybe "" n)

    -- Lets wire up some events
    on numInput keyDown (liftIO setNext)
    on numInput keyUp (liftIO setNext)
    on numInput keyPress (liftIO setNext)
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

