{-# LANGUAGE QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

{-
   start 10000 threads that each randomly update the color of a single cell in a table
-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import System.Random

import GHCJS.Foreign.QQ
import GHCJS.Types

addStyle :: [JSString] -> IO ()
addStyle styles = do
  (sh :: JSRef ()) <-
        [jsu| var st = document.createElement('style');
              st.appendChild(document.createTextNode(''));
              document.head.appendChild(st);
              $r = st.sheet;
            |]
  forM_ styles $ \s -> [jsu_| `sh.insertRule(`s, 0); |]

addChild :: JSRef () -> JSString -> IO (JSRef ())
addChild parent tagName =
  [jsu| var elem = document.createElement(`tagName);
         `parent.appendChild(elem);
         $r = elem;
      |]

setCol :: JSRef () -> Int -> IO ()
setCol elem col = [jsu_| `elem.className = 'col-' + `col; |]

main :: IO ()
main = do
  let dim = 100
  addStyle [ "body { background-color: #666; }"
           , "table { border-collapse: collapse; }"
           , "td { width: 7px; height: 7px; padding: 0; margin: 0; border: none; }"
           , "td.col-0 { background-color: #000; }", "td.col-1 { background-color: #444; }"
           , "td.col-2 { background-color: #888; }", "td.col-3 { background-color: #bbb; }"
           , "td.col-4 { background-color: #fff; }"
           ]
  table <- addChild [jsu'| document.body |] "table"
  rows  <- replicateM dim (addChild table "tr")
  cells <- concat <$> forM rows (\r -> replicateM dim (addChild r "td"))
  forM_ cells (void . forkIO . cellThread)

cellThread :: JSRef () -> IO a
cellThread elem = forever $ do
  setCol elem =<< randomRIO (0,4)
  threadDelay . (1000000+) =<< randomRIO (0,10000000)
