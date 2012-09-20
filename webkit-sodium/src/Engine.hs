module Engine where

import Graphics.UI.Gtk.WebKit.WebView
       (webViewNew, webViewGetDomDocument)
import Graphics.UI.Gtk.WebKit.DOM.Document
       (documentCreateElement, documentGetElementById, documentGetBody)
import Graphics.UI.Gtk.WebKit.DOM.HTMLElement
       (htmlElementInsertAdjacentElement, htmlElementSetInnerHTML,
        htmlElementInsertAdjacentHTML)
import Graphics.UI.Gtk.WebKit.Types (castToHTMLElement, Document, HTMLElement, ElementClass)
import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad.Trans ( liftIO )
import Graphics.UI.Gtk.General.Enums (WindowPosition(..))
import Graphics.UI.Gtk.WebKit.DOM.Element
import Graphics.UI.Gtk.WebKit.DOM.EventM
import Graphics.UI.Gtk.WebKit.DOM.Node
import Control.Monad
import Control.Monad.State.Strict
import Data.IORef

import FRP.Sodium
import Game

scale = 1/4
worldLeft = -1400
worldTop  = 1000
toWorld (x,y) = (x / scale + worldLeft, worldTop - y / scale)
fromWorldRect ((xOrig,yOrig),(wid,hei)) = (
        (
            (left - worldLeft) * scale,
            (worldTop - top) * scale
        ),
        (
            wid * scale * 2,
            hei * scale * 2
        )
    )
  where
    left = xOrig - wid
    top  = yOrig + hei

engine :: ElementClass elt =>
          Document -> elt -> (Event MouseEvent -> Reactive (BehaviorTree [Sprite])) -> IO (IO ()) 
engine doc div game = do
    (eMouse, pushMouse) <- sync newEvent

    let getXY = do
            (x0, y0) <- mouseClientXY
            liftIO $ do
                top <- elementGetOffsetTop div
                left <- elementGetOffsetLeft div
                return $ toWorld $ (fromIntegral *** fromIntegral) (x0 - left, y0 - top)

    elementOnmousedown div $ do
        xy <- getXY
        liftIO $ sync $ pushMouse $ MouseDown xy
    elementOnmousemove div $ do
        xy <- getXY
        liftIO $ sync $ pushMouse $ MouseMove xy
    elementOnmouseup div $ do
        xy <- getXY
        liftIO $ sync $ pushMouse $ MouseUp xy

    sprites <- sync (game eMouse)
    showAll doc div sprites

showAll :: ElementClass elt =>
             Document -> elt -> BehaviorTree [Sprite] -> IO (IO ())
showAll doc div sprites = evalStateT (showEach sprites) (-1000000)
  where
    showEach :: BehaviorTree [Sprite] -> StateT Int IO (IO ())
    showEach (left :++ right) = do
        unlisten1 <- showEach left
        unlisten2 <- showEach right
        return (unlisten1 >> unlisten2)
    showEach (BehaviorNode beh) = do
        zIxRoot <- get
        modify (1000+)
        liftIO $ do
            lastRef <- newIORef []
            unlisten <- sync $ listen (values beh) $ \these -> do
                last <- readIORef lastRef
                let toAdd = drop (length last) these
                    toRemove = drop (length these) (map fst last)
                    toModify = zip these last
                    keptSpans = map (\(_, (elt, _)) -> elt) toModify
                    noOfKeptSpans = length keptSpans
                mapM_ (nodeRemoveChild div . Just) toRemove
                sequence_ $ zipWith (\zIx (newSprite, (elt, oldSprite)) -> do
                        let (oldRect, oldFn) = oldSprite
                            (newRect, newFn) = newSprite
                        when (oldRect /= newRect) $ position  elt newRect zIx
                        when (oldFn /= newFn)     $ associate elt newFn
                    ) [zIxRoot..] toModify
                addedSpans <- sequence $ zipWith (\zIx sprite@(rect, fn) -> do
                        Just elt <- fmap castToHTMLElement <$> documentCreateElement doc "img"
                        position elt rect zIx
                        associate elt fn
                        elementSetAttribute elt "draggable" "false"
                        nodeAppendChild div (Just elt)
                        return elt
                    ) [zIxRoot + noOfKeptSpans..] toAdd
                writeIORef lastRef (zip (keptSpans ++ addedSpans) these)
            return unlisten
    position elt rect zIx = do
        elementSetAttribute elt "style" (style origin zIx)
        elementSetAttribute elt "width" (show wid)
        elementSetAttribute elt "height" (show hei)
      where
        (origin, (wid, hei)) = fromWorldRect rect
        style (x, y) zIx = "position:absolute;top:"++show y
                                    ++"px;left:"++show x
                                  ++"px;zIndex:"++show zIx
                                  ++disableTextSelection
        disableTextSelection =
            "-webkit-touch-callout:none;"++
            "-webkit-user-select: none;"++
            "-khtml-user-select: none;"++
            "-moz-user-select: none;"++
            "-ms-user-select: none;"++
            "user-select: none"
    associate elt fn =
        elementSetAttribute elt "src" ("http://localhost/"++fn)

