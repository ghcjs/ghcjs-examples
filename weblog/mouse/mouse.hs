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

#ifdef __GHCJS__
-- create an element in the SVG namespace
foreign import javascript unsafe "document.createElementNS('http://www.w3.org/2000/svg',$1)"
   createSvg :: JSString -> IO Element
foreign import javascript unsafe "document.getElementsByTagName($1)"
   getElementsByTagName :: JSString -> IO (JSArray a)
foreign import javascript unsafe "$3.setAttribute($1,$2)"
   setAttribute :: JSString -> JSRef a -> JSRef b -> IO ()
foreign import javascript unsafe "$2.appendChild($1)"
   appendChild :: Element -> Element -> IO ()
#else
createSvg = undefined
appendChild = undefined
getElementsByTagName = undefined
setAttribute = undefined
#endif

setAttribute' :: ToJSRef a => JSString -> a -> JSRef b -> IO ()
setAttribute' a v o = toJSRef v >>= \v' -> setAttribute a v' o

main = do
  body <- indexArray 0 =<< getElementsByTagName "body"
  svg <- createSvg "svg"
  appendChild svg body
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
  circle <- createSvg "circle"
  let p .= v = setAttribute' p v circle
  "fill" .= color >> "r" .= r
  appendChild circle parent
  sync $ listen (values x) $
    \(x,y) -> "cx" .= x >> "cy" .= y
  return ()

mousePosition :: Element -> IO (Behaviour (Double, Double))
mousePosition elem = do
  (b, push) <- sync $ newBehaviour (0,0)
  let handler ev = do
        x <- pageX ev
        y <- pageY ev
        sync $ push (x,y)
  on handler "mousemove" def =<< selectElement elem
  return b

