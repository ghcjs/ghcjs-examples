{-# LANGUAGE JavaScriptFFI, CPP #-}

module Main where

#ifdef __GHCJS__
foreign import javascript unsafe        "document.write($1+'<br/>');" writeNumber :: Int -> IO ()
foreign import javascript interruptible "setTimeout($c, $1);"         delay       :: Int -> IO ()
#else
writeNumber = error "writeNumber: only available from JavaScript"
delay = error "delay: only available from JavaScript"
#endif

main :: IO ()
main = mapM_ (\x -> writeNumber x >> delay 1000) [1..1000]

