{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad

-- this is inefficient on purpose
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib38 = fib 38

printFib = print fib38 `E.catch` \(_::E.SomeException) -> putStrLn "not finished"

main = forever $ do
         tid <- forkIO printFib
         threadDelay 1000000
         throwTo tid E.Overflow
