{-# LANGUAGE JavaScriptFFI, CPP #-}
module Main where

import Control.Monad
import Control.Concurrent
import GHCJS.Foreign
import GHCJS.Types

#ifdef __GHCJS__
foreign import javascript unsafe "document.getElementById($1).style.left = '' + $2 + 'px'"
  setPos :: JSString -> Int -> IO ()
#else
setPos = error "setPos: only available in JavaScript"
#endif

main :: IO ()
main = mapM_ runRacer [1..10] 

runRacer :: Int -> IO ()
runRacer n = void $ forkIO $ do
  doRace (toJSString $ "racer" ++ show n)

doRace :: JSString -> IO ()
doRace str = go (0::Int)
  where
    go n | n > 800   = go 0
         | otherwise = do
             setPos str n
             threadDelay 1
             go (n+1)

