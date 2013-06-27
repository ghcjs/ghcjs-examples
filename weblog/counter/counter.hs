{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T

import           JavaScript.JQuery hiding (Event)

import           FRP.Sodium

main :: IO ()
main = do
  body <- select "body"
  buttonEvent <- reactiveButton "Click Me!" body
  counterDiv <- select "<div />"
  appendJQuery counterDiv body
  sync $ do
    counter <- count buttonEvent
    listen (values counter) (\n -> void $
            setText (T.pack . show $ n) counterDiv)
  return ()

reactiveButton :: Text -> JQuery -> IO (Event ())
reactiveButton label parent = do
  (evt, a) <- sync newEvent
  button <- select "<button />"
  setText label button
  appendJQuery button parent
  let handler _ = sync (a ())
  on handler "click" def button
  return evt

