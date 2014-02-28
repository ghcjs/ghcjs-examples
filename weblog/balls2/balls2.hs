{-# LANGUAGE CPP, OverloadedStrings, TypeFamilies, JavaScriptFFI #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Default
import           Data.IORef
import           Data.VectorSpace
import           System.Random

import           FRP.Sodium

import           JavaScript.JQuery hiding (Event, animate)
import           GHCJS.Types
import           GHCJS.Foreign

#ifdef __GHCJS__
foreign import javascript unsafe "Date.now()" now :: IO Double
foreign import javascript unsafe "$3.css($1,$2+'px')"
  setCssPx :: JSString -> Double -> JQuery -> IO ()
foreign import javascript unsafe
  "var req = window.requestAnimationFrame ||\
             window.mozRequestAnimationFrame ||\
             window.webkitRequestAnimationFrame ||\
             window.msRequestAnimationFrame;\
   var f = function() { $1(); req(f); };\
   req(f);"
  animate :: JSFun (IO ()) -> IO ()
#else
now            = return 0
setCssPx _ _ _ = undefined
animate _      = undefined
#endif

data R2 = R2 { _x :: Double, _y :: Double }
  deriving (Show, Eq, Ord)

instance AdditiveGroup R2 where
  zeroV = R2 0 0
  R2 x1 y1 ^+^ R2 x2 y2 = R2 (x1+x2) (y1+y2)
  negateV (R2 x y) = R2 (negate x) (negate y)

instance VectorSpace R2 where
  type Scalar R2 = Double
  s *^ (R2 x y) = R2 (s*x) (s*y)

instance InnerSpace R2 where
  (R2 x1 y1) <.> (R2 x2 y2) = (x1*x2)+(y1*y2)

main :: IO ()
main = do
  body <- select "body"
  bodySize <- size body
  mouse <- mousePosition body
  startSize <- sync (sample bodySize)
  (stepper, pushStepper) <- sync newEvent
  let stepper' = snapshotWith (,) stepper ((,) <$> mouse <*> bodySize)
  replicateM_ 10 (startPos bodySize >>= \start -> ball body start stepper')
  t <- newIORef =<< now
  let step = do
        t0 <- readIORef t
        t1 <- now
        sync (pushStepper $ t1-t0)
        writeIORef t t1
  animate =<< syncCallback False False step

startPos :: Behaviour R2 -> IO R2
startPos size = do
  R2 mx my <- sync (sample size)
  R2 <$> randomRIO (0,mx) <*> randomRIO (0,my)

ball :: JQuery
     -> R2
     -> Event (Double, (R2, R2))
     -> IO (Behaviour R2)
ball parent startPos step = do
  b <- select "<img src='ball.png' width='25' height='25' />"
  setCss "position" "absolute" b
  appendJQuery b parent
  let updCss prop f x = void (setCssPx prop (f x) b)
  pos <- sync (hold startPos =<< collectE upd initial step)
  sync (listen (values pos) $ \x -> updCss "left" _x x >> updCss "top" _y x)
  return pos
    where
      initial = (startPos, R2 0 0)
      upd (dt,(m,s)) (x,v) =
        let r          = m ^-^ x
            a          = (5 * recip (300 + magnitudeSq r)) *^ normalized r
            t@(x', v') = clamp s 25 (x ^+^ (dt *^ v)) ((0.9995 ** dt) *^ (v ^+^ (dt *^ a)))
        in  (x',t)

clamp :: R2 -> Double -> R2 -> R2 -> (R2, R2)
clamp size objSize x v = (R2 xx xy, R2 vx vy)
  where
    (xx, vx) = clamp' _x
    (xy, vy) = clamp' _y
    clamp' f
      | x' < 0    = (-x', abs v')
      | x' > m    = (2 * m - x', negate (abs v'))
      | otherwise = (x', v')
      where
        x' = f x
        v' = f v
        m  = f size - objSize

-- size of the element, in pixels
size :: JQuery -> IO (Behaviour R2)
size elem = do
  (b, push) <- sync . newBehaviour =<< dims
  let handler _ = sync . push =<< dims
  on handler "resize" def elem
  return b
    where
      dims = R2 <$> getWidth elem
                <*> getHeight elem

-- the mouse position, in pixels from the top-left corner
mousePosition :: JQuery -> IO (Behaviour R2)
mousePosition elem = do
  (b, push) <- sync $ newBehaviour (R2 0 0)
  let handler ev = do
        x <- pageX ev
        y <- pageY ev
        sync $ push (R2 x y)
  on handler "mousemove" def elem
  return b
