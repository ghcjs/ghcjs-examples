{-# LANGUAGE TemplateHaskell, QuasiQuotes, ViewPatterns #-}
module Main (
    compileFromTo
  , main
) where

import Language.Fay
import Language.Fay.Types
import Paths_fay_hello_js
import Control.Exception (throw)
import Control.Monad.IO.Class (liftIO)
import Language.Haskell.TH (litE, stringL, runIO)
import System.Environment (getArgs)


-- import Paths_fay as Fay
-- fayPath = Fay.getDataFileName

main = getBinDir >>= putStrLn

-- Based on code in the fay package Language.Fay.Compiler ....

-- | Compile file program to…
compileFromTo :: Bool -> String -> IO String
compileFromTo autorun hscode = do
  let raw = $((runIO $ readFile "/Users/hamish/Library/Haskell/ghc-7.4.1.20120701/lib/fay-0.1.0.0/share/js/runtime.js") >>= litE . stringL)
  let stdlib = $((runIO $ readFile "/Users/hamish/Library/Haskell/ghc-7.4.1.20120701/lib/fay-0.1.0.0/share/hs/stdlib.hs") >>= litE . stringL)
  let stdlibprelude = $((runIO $ readFile "/Users/hamish/Library/Haskell/ghc-7.4.1.20120701/lib/fay-0.1.0.0/share/src/Language/Fay/Stdlib.hs") >>= litE . stringL)
  result <- compileProgram autorun raw compileModule (hscode ++ "\n" ++ stdlib ++ "\n" ++ strip stdlibprelude)
  case result of
    Right out -> return out
    Left  err -> return $ show err

  where strip = unlines . dropWhile (/="-- START") . lines

-- | Compile the given module to a runnable program.
compileProgram :: (Show from,Show to,CompilesTo from to)
               => Bool -> String -> (from -> Compile to) -> String
               -> IO (Either CompileError String)
compileProgram autorun raw with hscode = do
  result <- compileViaStr with hscode
  case result of
    Left err -> return (Left err)
    Right jscode -> return (Right (unlines ["var Fay = function(){"
                                           ,raw
                                           ,jscode
                                           ,"return {"
                                           ,"  force:_,"
                                           ,"  thunk:$,"
                                           ,"  list:Fay$$list,"
                                           ,"  encodeShow:Fay$$encodeShow,"
                                           ,"  main:main,"
                                           ,"  eval:Fay$$eval"
                                           ,"  };"
                                           ,"};"
                                           ,if autorun
                                               then ";\nvar fay = new Fay();fay.force(fay.main);"
                                               else ""
                                           ]))

-- | Convert a Haskell filename to a JS filename.
toJsName :: String -> String
toJsName x = case reverse x of
               ('s':'h':'.': (reverse -> file)) -> file ++ ".js"
               _ -> x

