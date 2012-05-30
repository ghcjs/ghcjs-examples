{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, MagicHash, OverloadedStrings #-}
module JS (
    hello
  , validatePrime
  , tryHamlet
) where

import Text.Hamlet (defaultHamletSettings, shamlet, Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy (Text, pack, append)
import Text.Hamlet.RT (renderHamletRT, parseHamletRT)
import GHC.Prim
import GHC.CString (unpackCString#)

type CString = Addr#

-- This example is here to show that you can create
-- small simple functions if that is what you want.
-- The function level linking and bundling will
-- minimize the amount of Java Script downloaded
-- when they are called.
hello :: CString -> Text
hello x = "Hello " `append` pack (unpackCString# x)

-- This "validatePrime" example uses
--  * integer-gmp (implemented using goog.math.Integer)
--  * Text
--  * Hamlet template haskell
isPrime :: Integral a => a -> Bool
isPrime p = p > 1 && (all (\n -> p `mod` n /= 0)
                     $ takeWhile (\n -> n*n <= p) [2..])

-- The Hamlet used here is a compile time template
validatePrime' :: Integer -> Html
validatePrime' p | isPrime p = [shamlet|<b>Yes</b>, #{p} is a prime|]
                 | otherwise = [shamlet|<b>No</b>, #{p} is not a prime|]

validatePrime :: CString -> Text
validatePrime n = renderHtml . validatePrime' . read $ unpackCString# n

-- This "tryHaskell" example runs the Hamlet parser on the client
-- this needs more JS code than the rest, but it is not loaded until
-- it is used.
tryHamlet :: CString -> IO Text
tryHamlet h = do
    rt   <- parseHamletRT defaultHamletSettings (unpackCString# h)
    html <- renderHamletRT rt [] (\_ _ -> "")
    return $ renderHtml html


