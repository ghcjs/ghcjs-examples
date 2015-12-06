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

import GHCJS.DOM
import GHCJS.DOM.Types
       -- (HTMLImageElement(..), castToHTMLImageElement, GType, isA,
       --  gTypeHTMLImageElement, castToHTMLElement, gTypeHTMLElement,
       --  HTMLElement(..), HTMLCanvasElement(..), castToHTMLCanvasElement,
       --  gTypeHTMLCanvasElement, HTMLInputElement(..), HTMLDivElement(..),
       --  Element(..), DocumentClass, castToHTMLInputElement,
       --  gTypeHTMLInputElement, castToHTMLDivElement, gTypeHTMLDivElement)
import GHCJS.DOM.Document hiding (getElementById, error, createElement)
import qualified GHCJS.DOM.Document as GHCJSDoc
       -- (documentCreateElement, documentGetElementById)


getHTMLElementById :: IsDocument doc => doc -> String -> IO HTMLElement
getHTMLElementById = getElementById gTypeHTMLElement castToHTMLElement

getDivElementById :: IsDocument doc => doc -> String -> IO HTMLDivElement
getDivElementById = getElementById gTypeHTMLDivElement castToHTMLDivElement

getInputElementById :: IsDocument doc => doc -> String -> IO HTMLInputElement
getInputElementById = getElementById gTypeHTMLInputElement castToHTMLInputElement

getImageElementById :: IsDocument doc => doc -> String -> IO HTMLImageElement
getImageElementById = getElementById gTypeHTMLImageElement castToHTMLImageElement

getElementById :: IsDocument doc
               => GType          -- ^ GObject type we want
               -> (Element -> a) -- ^ Suitable cast operator
               -> doc            -- ^ Document
               -> String         -- ^ ID of the element
               -> IO a
getElementById gtype cast doc elementId = do
    maybeElement <- GHCJSDoc.getElementById doc elementId
    case maybeElement of
        Just e | e `isA` gtype -> return (cast e)
        Just _  -> error $ "Element '" ++ elementId ++ "' found, but wrong type"
        Nothing -> error $ "Element '" ++ elementId ++ "' not found"

createDivElement :: IsDocument doc => doc -> IO HTMLDivElement
createDivElement = createElement gTypeHTMLDivElement castToHTMLDivElement "div"

createCanvasElement :: IsDocument doc => doc -> IO HTMLCanvasElement
createCanvasElement = createElement gTypeHTMLCanvasElement castToHTMLCanvasElement "canvas"

createElement :: IsDocument doc
              => GType          -- ^ GObject type we want
              -> (Element -> a) -- ^ Suitable cast operator
              -> String         -- ^ element type
              -> doc            -- ^ Document
              -> IO a
createElement gtype cast elementType doc = do
    maybeElement <- GHCJSDoc.createElement doc (Just elementType)
    case maybeElement of
        Just e | e `isA` gtype -> return (cast e)
        Just _  -> error $ "Created an '" ++ elementType ++ "', but it was the wrong type"
        Nothing -> error $ "Failed to create a '" ++ elementType ++ "'"


