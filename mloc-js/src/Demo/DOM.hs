{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Demo.DOM
-- Copyright   :
-- License     :  BSD3
--
-- | Manipulating the DOM is an important part of any web application.
--   To find a good way to do this we took a step back and asked how
--   it would be done if we were writing just a native haskell application.
--   You could write an entire web browser in Haskell, but a quicker solution
--   is to write bindings to an existing browser engine.
--
--   We chose WebKit as the WebKitGTK+ library was the easiest to write
--   haskell bindings for (it's C functions not C++).
--
--   We adapted an existing IDL process tool so that we could make the Haskell
--   bindings from the WebKit IDL files.
--
--   Then we did the same to output JavaScript replacements for the C functions
--   that make up WebKitGTK+.
--
-----------------------------------------------------------------------------

module Demo.DOM (
    helloDOM
) where

import GHCJS.DOM.Document (documentGetElementById)
import GHCJS.DOM.HTMLElement
       (htmlElementSetInnerText)
import GHCJS.DOM.Types (castToHTMLElement)

helloDOM doc = do
    maybeExample <- documentGetElementById doc "example"
    case maybeExample of
        Just example -> htmlElementSetInnerText
                                (castToHTMLElement example)
                                "Hello World"
        Nothing      -> putStrLn "Element 'example' not found"


#ifdef SAMPLE_CODE

        -- This function is from the WebKitGTK+ Haskell bindings (generated
        -- from the WebKit IDL files.  It is included here to give an idea
        -- of how these bindings work.  webkit_dom_document_get_element_by_id
        -- is the underlying C function called.
        -- When compiled to JavaScript the C function will be emulated
        -- with a JavaScript one.
        documentGetElementById ::(DocumentClass self)
                               => self
                               -> String
                               -> IO (Maybe Element)
        documentGetElementById self elementId
          = maybeNull (makeNewGObject mkElement)
              (withUTFString elementId $
                 \ elementIdPtr ->
                   {# call webkit_dom_document_get_element_by_id #}
                        (toDocument self)
                        elementIdPtr)

#endif
