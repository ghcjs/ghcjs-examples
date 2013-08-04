{-# LANGUAGE DeriveFoldable, TemplateHaskell, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction, Rank2Types #-}
-----------------------------------------------------------------------------
--
-- Module      :  Demo.Life
-- Copyright   :
-- License     :  BSD3
--
-- |
--
-----------------------------------------------------------------------------

module Demo.Life (
    life
) where

import GHCJS.DOM.Types
       (HTMLDivElement(..), Document(..), WebView(..))
import GHCJS.DOM.HTMLElement
       (htmlElementSetInnerHTML)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import GHCJS.DOM (webViewGetDomDocument)
import Control.Monad.Reader (ReaderT(..))
import Language.Javascript.JSC
       (MakeValueRef(..), MakeStringRef(..), JSF(..), runJSC,
        jsg, js, js1, js4, (<#))
import Control.Lens (IndexPreservingGetter, Getter, to, (^.))
import qualified Data.Foldable as F (Foldable(..), Foldable)
import Graphics.Gloss
       (Picture(..), Display(..), Color(..), black, red,
        color, translate, rectangleSolid)
import Graphics.Gloss.Interface.Pure.Simulate (ViewPort)
import Data.Array (Array(..), (!), indices, bounds, listArray)
import Data.List (transpose, isPrefixOf)
import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(..), (<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Trans ( liftIO )
import GHC.Float (float2Double)

life :: WebView -> Document -> HTMLDivElement -> IO ()
life webView doc example = do
    let canvasStyle = "position:absolute; top:60; left:300; visible: hidden"
    htmlElementSetInnerHTML example . unpack $ renderHtml
        [shamlet|$newline always
            <div #"lifeDiv">
                <canvas #"canvas0" width="400" height="400" style="#{canvasStyle}">
                <canvas #"canvas1" width="400" height="400" style="#{canvasStyle}">
        |]

    let getElementById :: (MakeValueRef s, MakeStringRef s) => s -> JSF
        getElementById = js1 "getElementById"
        getContext     = js1 "getContext"
        fillStyle      = js "fillStyle"
        fillRect :: Double -> Double -> Double -> Double -> JSF
        fillRect       = js4 "fillRect"
        style          = js "style"
        visibility     = js "visibility"

    runJSC webView $ do
        document <- jsg "document"

        canvas0 <- document ^. getElementById "canvas0"
        canvas1 <- document ^. getElementById "canvas1"

        let simulate n (canvas0, canvas1) speed model = do
                runJSC webView $ do
                    ctx <- canvas0 ^. getContext "2d"
                    ctx ^. fillStyle <# "#FFFFFF" -- if n `mod` 2 == 0 then "#FFFFFF" else "#AA0000"
                    ctx ^. fillRect 0 0 600 400
                    drawPicture ctx (300,200) (render model)
                    canvas1 ^. style . visibility <# "hidden"
                    canvas0 ^. style . visibility <# "visible"
                threadDelay (1000000 `div` speed)
                simulate (n+1) (canvas1, canvas0) speed (step () 1.0 model)
            drawPicture ctx orig Blank = return ()
            drawPicture ctx orig (Pictures pics) = mapM_ (drawPicture ctx orig) pics
            drawPicture ctx (ox,oy) (Translate x y pic) = drawPicture ctx (ox+x, oy+y) pic
            drawPicture ctx (ox,oy) (Polygon [(a,b), _, (c,d), _]) = do
                ctx ^. fillRect (realToFrac $ ox+a) (realToFrac $ oy+b)
                                (realToFrac $ c-a)  (realToFrac $ d-b)
                return ()
            drawPicture ctx orig (Color c pic) = do
                ctx ^. fillStyle <# "#0000C0"
                drawPicture ctx orig pic
            drawPicture ctx orig p = liftIO $ print p
        liftIO . forkIO $ simulate 0 (canvas0, canvas1) 10 (parseGrid $ unlines
                [
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "...........................O..............",
                ".........................O.O..............",
                "...............OO......OO............OO...",
                "..............O...O....OO............OO...",
                "...OO........O.....O...OO.................",
                "...OO........O...O.OO....O.O..............",
                ".............O.....O.......O..............",
                "..............O...O.......................",
                "...............OO.........................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                "..........................................",
                ".........................................."]
            )
        return ()



-- What follows is code from a Hoodlums session
-- https://github.com/petermarks/hoodlums-sessions/blob/master/Life.hs
data Grid a = Grid (Array (Int,Int) a) (Int, Int)
  deriving F.Foldable

class Comonad w where
  extract :: w a -> a
  (=>>) :: w a -> (w a -> b) -> w b

instance Comonad Grid where
  extract (Grid a p) = a ! p
  (Grid a p) =>> f   = Grid ( listArray (bounds a) . map (f . Grid a) $ indices a) p

--main :: IO ()
--main = do
--  gridString <- readFile "grid.life"
--  let grid = parseGrid gridString
--  runGame grid

parseGrid :: String -> Grid Bool
parseGrid s = Grid ( listArray ((1,1), (width, height)) es) (1,1)
  where
    ls     = dropWhile ("!" `isPrefixOf`) $ lines s
    width  = length $ head ls
    height = length ls
    es     =  map (== 'O') $ concat $ transpose ls

--runGame :: Grid Bool -> IO ()
--runGame grid = simulate (InWindow "Life" (windowSize grid) (10,10))
--                black 10 grid render step

size :: Grid a -> (Int, Int)
size (Grid a _) = snd (bounds a)

render :: Grid Bool -> Picture
render g = color red $ F.fold $ g =>> renderCell

gridIndex :: Grid a -> (Int, Int)
gridIndex (Grid _ p) = p

gridIndices :: Grid a -> Grid (Int, Int)
gridIndices g = g =>> gridIndex

renderCell :: Grid Bool -> Picture
renderCell g | alive = translate (fromIntegral $ x * 10 - w * 5 - 15)
                                 (fromIntegral $ y * (-10) + h * 5 + 15) $
                                 rectangleSolid 8 8
             | otherwise = mempty
  where
    alive = extract g
    (x, y) = gridIndex g
    (w, h) = size g

windowSize :: Grid a -> (Int, Int)
-- windowSize g = (x * 10, y * 10)
--   where (x, y) = size g
windowSize _ = (500, 500)

rule :: Bool -> Int-> Bool
rule True  i = i == 2 || i == 3
rule False i = i == 3

moveGrid :: (Int, Int) -> Grid a -> Grid a
moveGrid (xx,yy) (Grid a (x,y)) = Grid a (x',y')
  where
    (w,h) = snd $ bounds a
    x' = (x + xx - 1) `mod` w + 1
    y' = (y + yy - 1) `mod` h + 1

neighbours :: Grid Bool -> Int
neighbours g = length . filter id $ bools
  where
    bools = map (\o -> extract $ moveGrid o g) offsets
    offsets = [(x,y) | x <- [(-1)..1], y <- [(-1)..1], (x,y) /= (0,0)]

step :: x -> Float -> Grid Bool -> Grid Bool
step _ _ g = g =>> (rule <$> extract <*> neighbours)


