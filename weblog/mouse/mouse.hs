{-# LANGUAGE CPP, JavaScriptFFI, ForeignFunctionInterface,
             EmptyDataDecls, OverloadedStrings,
             ScopedTypeVariables
  #-}
module Main where

import           Control.Applicative
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T

import           GHCJS.Types
import           GHCJS.Foreign
import           GHCJS.Marshal
import           JavaScript.JQuery

import           FRP.Sodium
import           GHCJS.DOM (runWebGUI, currentDocument)
import           GHCJS.DOM.DOMWindow (domWindowGetDocument)
import           GHCJS.DOM.Types (HTMLElement, Element, unHTMLElement,
                    ElementClass(..))
import           GHCJS.DOM.Document (documentCreateElementNS,
                    documentGetElementsByTagName, documentGetBody)
import           GHCJS.DOM.Node (nodeAppendChild)
import           GHCJS.DOM.Element (elementSetAttribute)

-- | create an element in the SVG namespace
createSvg :: String -> IO (Maybe Element)
createSvg n = do
    Just doc <- currentDocument
    documentCreateElementNS doc ("http://www.w3.org/2000/svg"::String) n

setAttribute' :: (ElementClass o, ToJSRef v) => JSString -> v -> o -> IO ()
setAttribute' a v o = toJSRef v >>= \v' -> elementSetAttribute o a (castRef v' :: JSString)

main =
  runWebGUI $ \ webView -> do
    Just doc <- domWindowGetDocument webView
    Just body <- documentGetBody doc
    Just svg <- createSvg "svg"
    nodeAppendChild body (Just svg)
    setAttribute' "width" (400::Int) svg >> setAttribute' "height" (400::Int) svg
    t <- fmap ((*5) . fst) <$> mousePosition body
    let sun   = pure (200, 200)
        earth = object (1/365) 150 sun   t
        moon  = object (1/30)  25  earth t
    drawObject svg "yellow" 20 sun
    drawObject svg "blue"   8  earth
    drawObject svg "grey"   3  moon

object :: Double
       -> Double
       -> Behaviour (Double, Double)
       -> Behaviour Double
       -> (Behaviour (Double, Double))
object speed r center time =
   (,) <$> liftA2 xpos center time <*> liftA2 ypos center time
     where
       xpos (x,_) t = x + r * cos (speed * t)
       ypos (_,y) t = y + r * sin (speed * t)

drawObject :: Element -> Text -> Double -> Behaviour (Double, Double) -> IO ()
drawObject parent color r x = do
  putStrLn (T.unpack color)
  Just circle <- createSvg "circle"
  let p .= v = setAttribute' p v circle
  "fill" .= color >> "r" .= r
  nodeAppendChild parent (Just circle)
  sync $ listen (values x) $
    \(x,y) -> "cx" .= x >> "cy" .= y
  return ()

mousePosition :: ElementClass e => e -> IO (Behaviour (Double, Double))
mousePosition elem = do
  (b, push) <- sync $ newBehaviour (0,0)
  let handler ev = do
        x <- pageX ev
        y <- pageY ev
        sync $ push (x,y)
  on handler "mousemove" def =<< selectElement elem
  return b

