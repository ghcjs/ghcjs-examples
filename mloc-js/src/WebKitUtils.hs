-----------------------------------------------------------------------------
--
-- Module      :  WebKitUtils
-- Copyright   :
-- License     :  BSD3
--
-- |
--
-----------------------------------------------------------------------------

module WebKitUtils (
    getHTMLElementById
  , getDivElementById
  , getInputElementById
  , getImageElementById
  , createDivElement
  , createCanvasElement
) where

import Graphics.UI.Gtk.WebKit.Types
       (HTMLImageElement(..), castToHTMLImageElement,
        gTypeHTMLImageElement, castToHTMLElement, gTypeHTMLElement,
        HTMLElement(..), HTMLCanvasElement(..), castToHTMLCanvasElement,
        gTypeHTMLCanvasElement, HTMLInputElement(..), HTMLDivElement(..),
        Element(..), DocumentClass, castToHTMLInputElement,
        gTypeHTMLInputElement, castToHTMLDivElement, gTypeHTMLDivElement)
import Graphics.UI.Gtk.WebKit.DOM.Document
       (documentCreateElement, documentGetElementById)
import Graphics.UI.Gtk (GType, isA)


getHTMLElementById :: DocumentClass doc => doc -> String -> IO HTMLElement
getHTMLElementById = getElementById gTypeHTMLElement castToHTMLElement

getDivElementById :: DocumentClass doc => doc -> String -> IO HTMLDivElement
getDivElementById = getElementById gTypeHTMLDivElement castToHTMLDivElement

getInputElementById :: DocumentClass doc => doc -> String -> IO HTMLInputElement
getInputElementById = getElementById gTypeHTMLInputElement castToHTMLInputElement

getImageElementById :: DocumentClass doc => doc -> String -> IO HTMLImageElement
getImageElementById = getElementById gTypeHTMLImageElement castToHTMLImageElement

getElementById :: DocumentClass doc
               => GType          -- ^ GObject type we want
               -> (Element -> a) -- ^ Suitable cast operator
               -> doc            -- ^ Document
               -> String         -- ^ ID of the element
               -> IO a
getElementById gtype cast doc elementId = do
    maybeElement <- documentGetElementById doc elementId
    case maybeElement of
        Just e | e `isA` gtype -> return (cast e)
        Just _  -> error $ "Element '" ++ elementId ++ "' found, but wrong type"
        Nothing -> error $ "Element '" ++ elementId ++ "' not found"

createDivElement :: DocumentClass doc => doc -> IO HTMLDivElement
createDivElement = createElement gTypeHTMLDivElement castToHTMLDivElement "div"

createCanvasElement :: DocumentClass doc => doc -> IO HTMLCanvasElement
createCanvasElement = createElement gTypeHTMLCanvasElement castToHTMLCanvasElement "canvas"

createElement :: DocumentClass doc
              => GType          -- ^ GObject type we want
              -> (Element -> a) -- ^ Suitable cast operator
              -> String         -- ^ element type
              -> doc            -- ^ Document
              -> IO a
createElement gtype cast elementType doc = do
    maybeElement <- documentCreateElement doc elementType
    case maybeElement of
        Just e | e `isA` gtype -> return (cast e)
        Just _  -> error $ "Created an '" ++ elementType ++ "', but it was the wrong type"
        Nothing -> error $ "Failed to create a '" ++ elementType ++ "'"


