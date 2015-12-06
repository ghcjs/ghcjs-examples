-----------------------------------------------------------------------------
--
-- Module      :  Demo.LazyLoading
-- Copyright   :  (c) Hamish Mackenzie, Luite Stegeman
-- License     :  MIT
--
-- | For large applications it is usefule to strip out unused code and move
--   code that is not always needed into separate files.
--
--   The GHCJS linker performs Forest Shaking.  The function call graph is
--   a directed graph.  Given a set of root nodes (entry point functions
--   that we want to trigger code loading), it sorts the functions into files
--   such that we refer to as bundles.
--
--    * No function is in more than one bundle.
--
--    * Functions called by the same set of roots are put in the same bundle.
--
--    * No bundle is smaller than a threshold size (the smallest files are merged).
--
--   The goal is to minimize the amount of JavaScript that needs to be loaded
--   while avoiding having too many small files fetched from the server.
--
-----------------------------------------------------------------------------

module Demo.LazyLoading (
    lazyLoad_freecell
) where

import GHCJS.DOM.Element (setInnerHTML)
import Engine (engine)
import Freecell (mkFreecell)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)

-- | This function loads a FRP demo written by Stephen Blackheath.
--   The NOINLINE pragma tells GHC not to inline it (which would
--   make it imposible for the linker to replace it).  The linker
--   detects the lazyLoad_ prefix and replaces the function with
--   a loader function that fetches the bundles.
{-# NOINLINE lazyLoad_freecell #-}
lazyLoad_freecell webView doc example = do
    setInnerHTML example $ Just $
      "<div style=\""++style++"\" "++
      "id=\"freecell\" draggable=\"false\"></div>"
    unlisten <- engine webView "freecell" =<< mkFreecell
    -- Prevent finalizers running too soon
    forkIO $ forever (threadDelay 1000000000) >> unlisten
    return ()
  where
    style = "position:relative;left:0px;top:0px;"
            ++ "background-color:#e0d0ff;width:700px;height:500px"



