{-# LANGUAGE CPP, DeriveFoldable, TemplateHaskell, QuasiQuotes, ScopedTypeVariables, NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
--
-- Module      :  Demo.JavaScriptFFI
-- Copyright   :
-- License     :  BSD3
--
-- | To find a nice way to wor
--
-----------------------------------------------------------------------------

module Demo.JavaScriptFFI (
    canvasDemo
  , callHaskell
) where

import GHCJS.DOM (WebView(..))
import GHCJS.DOM.Types (Document(..), HTMLDivElement(..))
import GHCJS.DOM.HTMLCanvasElement (setHeight, setWidth)
import GHCJS.DOM.Node (appendChild)
import Control.Lens ((^.))
import Language.Javascript.JSaddle
       (eval, valToNumber, fun, jsg, js, (#), (<#), runJSaddle_, global)
import WebKitUtils
import GHCJS.DOM (webViewGetDomDocument)
import Control.Monad.Reader (ReaderT(..))
import GHCJS.DOM.Element (setInnerHTML)
import Data.Text.Lazy (unpack)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet (shamlet)
import Control.Monad (void)
import Control.Monad.Trans ( liftIO )
import Demo.Threading (isPrime)
#ifdef MIN_VERSION_jmacro
import Language.Javascript.JMacro
#endif

canvasDemo :: WebView -> Document -> HTMLDivElement -> IO ()
canvasDemo webView doc example = do
    setInnerHTML example . Just . unpack $ renderHtml
        [shamlet|$newline always
            <canvas #"canvas" width="600" height="400">
        |]

    runJSaddle_ webView $ do
        document <- jsg "document"
        let getElementById = js "getElementById"
            getContext     = js "getContext"
            fillStyle      = js "fillStyle"
            fillRect       = js "fillRect"

        -- var canvas = document.getElementById("canvas")
        canvas <- (#) document "getElementById" ["canvas"]
        -- var ctx = canvas.getContext("2d")
        ctx <- (#) canvas "getContext" ["2d"]
        -- ctx.fillStyle = "#00FF00"
        (ctx <# "fillStyle") "#008000"
        -- ctx.fillRect( 0, 0, 150, 75 )
        (#) ctx "fillRect" ([0, 0, 100, 100] :: [Double])

callHaskell :: WebView -> IO ()
callHaskell webView = do
    runJSaddle_ webView $ do
        (global <# "checkPrime") (fun $ \ f this [a] -> do
            num <- valToNumber a
            let i = round num
            liftIO . putStrLn $ "The number " ++ show i ++
                if isPrime i
                    then " is a prime"
                    else " is not a prime")
        eval "for(n = 0; n != 10; ++n) checkPrime(n);"
