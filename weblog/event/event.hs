{-# LANGUAGE OverloadedStrings #-}

module Main where

import JavaScript.JQuery
import JavaScript.JQuery.Internal
import Data.Default
import qualified Data.Text as T

main = do
  b <- select "body"
  append "<div class='mouse'></div>" b
  m <- select ".mouse"
  mousemove (handler m) def b

handler :: JQuery -> Event -> IO ()
handler m e = do
  x <- pageX e
  y <- pageY e
  setText (T.pack $ show (x,y)) m
  return ()

