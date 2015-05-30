{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, JavaScriptFFI, QuasiQuotes, Rank2Types #-}

{-
  A simple example demonstrating ghcjs-ffiqq and working with asynchronous threads
 -}
module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader

import qualified Language.PureScript as P

import           GHCJS.Foreign.QQ

setError :: String -> IO ()
setError xs = [js_| tryps.setError(`xs); |]

compileWorker :: [P.Module] -> MVar String -> (forall a. IO a -> IO a) -> IO ()
compileWorker prel mv unmask =
  forever $ unmask doCompile `catch` \(e::AsyncException) -> return ()
    where
      compileOpts =
        P.defaultCompileOptions { P.optionsAdditional    = P.CompileOptions "PS" [] []
                                , P.optionsVerboseErrors = True
                                }
      doCompile = do
        src  <- takeMVar mv
        [js_| tryps.setBusy(); |]
        case P.parseModulesFromFiles (const "<editor>") [((), src)] of
          Left err -> setError (show err)
          Right [] -> setError "no input"
          Right ms -> case P.compile (map snd ms ++ prel) [] `runReaderT` compileOpts of
             Left err -> setError err
             Right (jsSrc, _, _) -> [js_| tryps.setResult(`jsSrc); |]

abortCompilation :: ThreadId -> IO ()
abortCompilation worker = killThread worker

main :: IO ()
main = do
  [js_| trypsInit(); |]
  prelSrc <- [js| tryps.getPrelude() |]
  let prel = either (error . show) (map snd) $
             P.parseModulesFromFiles (const "<built-in>") [((), prelSrc)]
  mv <- newEmptyMVar
  worker <- mask_ $ forkIOWithUnmask (compileWorker prel mv)
  forever $ do
    [jsi_| tryps.waitForChange($c); |]
    abortCompilation worker
    threadDelay 500000
    putMVar mv =<< [js| tryps.getEditorContents() |]

