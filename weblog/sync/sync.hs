{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHCJS.Types
import GHCJS.Foreign

import Data.Text (Text)
import Control.Applicative
import Text.Read (readMaybe)

main = do
  o <- newObj
  setProp ("input"::Text) ("123"::JSString) o
  factorial o
  r <- getProp ("output"::Text) o
  putStrLn (fromJSString r)

factorial :: JSRef () -> IO ()
factorial ref = do
  x <- fromJSString <$> getProp ("input"::Text) ref
  let r = case readMaybe x of
            Just n | n < 0 || n > 5000 -> "invalid input"
            Just n -> toJSString . show . fact $ n
            Nothing -> "parse error"
  setProp ("output"::Text) r ref
  where
    fact :: Integer -> Integer
    fact n = product [1..n]
