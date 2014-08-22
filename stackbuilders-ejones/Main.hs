-- an example from
-- http://www.stackbuilders.com/news/the-modular-functional-client-side

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy as L
import           Data.IORef

import           FRP.Sodium

import           Text.Blaze.Html5 (Html, toHtml, style)
import           Text.Blaze.Html.Renderer.Utf8

import           JavaScript.JQuery hiding (Event)
import qualified JavaScript.JQuery as JQ

import Style
import Template

main :: IO ()
main = do
  select "head" >>= (append $ renderHtml' css)
  select "body" >>= (setHtml $ renderHtml' nameForm)

  firstNameInputField <- select "#firstNameInput"
  lastNameInputField  <- select "#lastNameInput"
  fullNameInputField  <- select "#fullNameInput"

  (firstNameEvents, publisherFn) <- sync newEvent
  (lastNameEvents, publisherFn') <- sync newEvent

  keyup (publishInputValues publisherFn) def firstNameInputField
  keyup (publishInputValues publisherFn') def lastNameInputField

  let fullNameEvents = firstNameEvents <> lastNameEvents

  sync $ listen fullNameEvents $ const $ do
    val  <- getVal firstNameInputField
    val' <- getVal lastNameInputField
    void $ setVal (val <> " " <> val') fullNameInputField

  return ()

publishInputValues :: (String -> Reactive ()) -> JQ.Event -> IO ()
publishInputValues f _ = sync $ f ""

renderHtml' :: Html -> Text
renderHtml' = decodeUtf8 . L.toStrict . renderHtml

css :: Html
css = style $ toHtml $ TL.toStrict $ cssText
