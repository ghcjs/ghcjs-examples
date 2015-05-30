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
compileWorker prelExts mv unmask =
  forever $ unmask doCompile `catch` \(e::AsyncException) -> return ()
    where
      doCompile = do
        src  <- takeMVar mv
        [js_| tryps.setBusy('Compiling...'); |]
        case P.parseModulesFromFiles (const "<editor>") [((), src)] of
          Left err -> setError (show err)
          Right [] -> setError "no input"
          Right ms ->
             let ms'  = map (P.runModuleName . P.getModuleName . snd) ms
                 opts = P.defaultCompileOptions
                            { P.optionsVerboseErrors = True
                            , P.optionsAdditional    = P.CompileOptions "PS" ms' ms'
                            }
             in case P.compile (prelExts ++ map snd ms) [] `runReaderT` opts of
                  Left err            -> setError err
                  Right (jsSrc, _, _) -> [js_| tryps.setResult(`jsSrc); |]

abortCompilation :: ThreadId -> IO ()
abortCompilation worker = killThread worker

main :: IO ()
main = do
  [js_| trypsInit();
        tryps.setBusy('Compiling Prelude...');
      |]
  prelSrc <- [js| tryps.getPrelude() |]
  let parsePS src = either (error . show) (map snd) $
                    P.parseModulesFromFiles (const "<built-in>") [((), src)]
      prelMods = parsePS prelSrc
      (prelJs, prelExtSrc, _prelEnv) =
         either error id $ P.compile prelMods [] `runReaderT` P.defaultCompileOptions
      prelExts = parsePS prelExtSrc
  [js_| tryps.setCompiledPrelude(`prelJs); |]
  mv <- newEmptyMVar
  worker <- mask_ $ forkIOWithUnmask (compileWorker prelExts mv)
  forever $ do
    [jsi_| tryps.waitForChange($c); |]
    abortCompilation worker
    threadDelay 500000
    putMVar mv =<< [js| tryps.getEditorContents() |]

