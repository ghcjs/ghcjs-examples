{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
module Main (
    main, lazyLoad_freecell
) where

import           Control.Applicative           ((<$>))
import           Control.Concurrent            (forkIO, forkIOWithUnmask,
                                                newEmptyMVar, putMVar, takeMVar,
                                                threadDelay, tryTakeMVar, MVar)
import           Control.DeepSeq               (deepseq)
import           Control.Exception             (Exception, SomeException, catch,
                                                throwTo)
import           Control.Lens                  ((^.))
import           Control.Monad                 (forever, when)
import           Control.Monad.Reader          (ReaderT (..))
import           Control.Monad.Trans           (liftIO)
import           Data.Maybe                    (fromMaybe)
import qualified Data.Text                     as T (pack, unpack)
import           Data.Text.Lazy                (Text, unpack)
import           Data.Typeable                 (Typeable)
import           Engine
import           Freecell
import           FRP.Sodium
import           GHCJS.DOM                     (postGUIAsync, postGUISync,
                                                runWebGUI,
                                                webViewGetDomDocument)
import           GHCJS.DOM.CSSStyleDeclaration (setProperty)
import           GHCJS.DOM.Document            (createElement, getBody,
                                                getElementById)
import           GHCJS.DOM.Element             (click, focus, getStyle, keyDown,
                                                keyPress, keyUp, setAttribute,
                                                setInnerHTML)
import           GHCJS.DOM.EventM
import           GHCJS.DOM.EventM              (mouseCtrlKey, mouseShiftKey)
import           GHCJS.DOM.HTMLElement         (setInnerText)
import           GHCJS.DOM.HTMLInputElement    (getValue)
import           GHCJS.DOM.Node                (appendChild, insertBefore)
import           GHCJS.DOM.Types               (Node (..), castToHTMLDivElement,
                                                castToHTMLElement,
                                                castToHTMLInputElement)
import           Language.Javascript.JSaddle   (JSF (..), JSM (..), JSNull (..),
                                                JSValue (..), array, call,
                                                deRefVal, eval, fun, global, js,
                                                js1, js4, jsg, new, runJSaddle_,
                                                strToText, val, valToNumber,
                                                valToObject, valToStr,
                                                valToText, (!), (!!), ( # ),
                                                (<#))
import           Prelude                       hiding ((!!))
import           System.IO                     (hFlush, hPutStrLn, stderr,
                                                stdout)
import           System.IO.Unsafe              (unsafePerformIO)
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Hamlet                   (shamlet)
#ifdef MIN_VERSION_jmacro
import           Language.Haskell.TH           (litE, stringL)
import           Language.Javascript.JMacro    (JStat (..), ToJExpr (..), jLam,
                                                jmacro, jmacroE, renderJs)
#endif

data NewValueException = NewValueException deriving (Show, Typeable)

instance Exception NewValueException

main = do
  -- Running a GUI creates a WebKitGtk window in native code,
  -- but just returns the browser window when compiled to JavaScript
  runWebGUI $ \ webView -> do
    -- WebKitGtk provides the normal W3C DOM functions
    Just doc <- webViewGetDomDocument webView
    Just body <- getBody doc

    -- Lets use some Hamlet to replace HTerm with some HTML
    Just div <- fmap castToHTMLDivElement <$> createElement doc (Just "div")
    setInnerHTML div . Just . unpack $ renderHtml [shamlet|$newline always
        <h1 #heading>
            Hello and Welcome GHCJS
        <p>
            Know any good prime numbers?
            <input #num size="8">
            <div #prime>
        <p>
            Here is a quick test of Canvas using jsc
            <canvas #"canvas" width="10" height="10">
        <ul>
          <li>
            Check out the <a href="https://github.com/ghcjs/ghcjs-examples/blob/master/ghcjs-hello/src/Main.hs">Haskell source code</a>
            \ for this example.  (read it carefully to find the hidden FRP example)
          <li>
            Try out the <a href="hterm.html">unminified version</a>
    |]
    -- Now we need to add this div to the document body
    -- If we are in the browser then let's shrink the terminal window to make room
    mbTerminal <- fmap castToHTMLDivElement <$> getElementById doc "terminal"
    case mbTerminal of
      Just terminal -> do
        Just style <- getStyle terminal
        setProperty style "height" (Just "200px") ""
        setProperty style "position" (Just "absolute") ""
        setProperty style "bottom" (Just "0") ""
        insertBefore body (Just div) (Just terminal)
      _             -> do
        appendChild body (Just div)

    -- We can get the elements by ID
    Just numInput <- fmap castToHTMLInputElement <$> getElementById doc "num"
    Just prime    <- fmap castToHTMLDivElement   <$> getElementById doc "prime"
    Just heading  <- fmap castToHTMLElement      <$> getElementById doc "heading"

    -- You can also use your favorite JavaScript libraries

    -- Run JavaScript using postGUISync to make sure it runs on the Gtk thread.
    -- This should avoid threading issues when using WebKitGTK+.
    let runjs = postGUIAsync . runJSaddle_ webView

    -- Declare the javascript property getters we will be using
    let getElementById = js1 "getElementById"
        getContext     = js1 "getContext"
        -- fillStyle      = js "fillStyle"
        fillRect :: Double -> Double -> Double -> Double -> JSF
        fillRect       = js4 "fillRect"
        get2dContext = do
            document <- jsg "document"
            -- var canvas = document.getElementById("canvas")
            -- return canvas.getContext("2d")
            canvas <- document ^. getElementById "canvas"
            canvas ^. getContext "2d"

    liftIO . forkIO . forever $ do
        runjs $ do
            ctx <- get2dContext
            -- ctx.fillStyle = "#00FF00"
            -- ctx.fillRect( 0, 0, 150, 75 )

            -- TODO: fix operator to be able to omit parentheses
            (ctx <# "fillStyle") "#00FF00"
            ctx ^. fillRect 0 0 10 10
        liftIO $ threadDelay 500000
        runjs $ do
            ctx <- get2dContext
            -- TODO: fix operator to be able to omit parentheses
            (ctx <# "fillStyle") "#FF0000"
            ctx ^. fillRect 0 0 10 10
        liftIO $ threadDelay 500000

    -- We don't want to work on more than on prime number test at a time.
    -- So we will have a single worker thread and a queue with just one value.
    next <- newEmptyMVar :: IO (MVar (Maybe String))
    ready <- newEmptyMVar
    worker <- forkIOWithUnmask $ \unmask -> forever $ unmask $ (do
              n <- takeMVar next
              postGUIAsync $ do
                  setInnerHTML prime $ Just ("Thinking about " ++ fromMaybe "nothing" n)
              let message = validatePrime n
              deepseq message $ postGUIAsync $ do
                  setInnerHTML prime . Just . unpack $ message)
         `catch` \ (e :: NewValueException) -> return ()

    -- Something to set the next work item
    let setNext = do
                    n <- getValue numInput
                    throwTo worker NewValueException
                    putMVar next n

    -- Lets wire up some events
    on numInput keyUp (liftIO setNext)
    on numInput keyPress (liftIO setNext)

    putStrLn "This is stdout."
    hPutStrLn stderr "This is stderr."
    putStrLn "You can get input from stdin."
    putStrLn "(we also support threads and MVar, so you can wait 20 seconds if you don't have a keyboard)"
    putStr   "What is your name ? "
    hFlush stdout

    -- We can use MVars and threads
    nameMVar <- newEmptyMVar :: IO (MVar String)

    -- Wait for input on one thread
    forkIO $ do
      line <- getLine
      putMVar nameMVar line

    -- Wait for 20s on another
    forkIO $ do
      threadDelay 20000000
      putMVar nameMVar "World"

    -- Get the first result
    forkIO $ do
      name <- takeMVar nameMVar
      postGUISync $ do
        setInnerText heading $ Just ("Hello " ++ name ++ " and Welcome GHCJS")

        -- Set the input focus to the prime number test
        focus numInput

        -- Now stdout is free let's try some more JavaScript stuff...
        runjs $ do
            -- Some helper functions to print JS values
            let log       v = deRefVal      v >>= (liftIO . putStrLn . showJSValue)
                logNumber v = valToNumber   v >>= (liftIO . putStrLn . show)
                logText   v = valToText     v >>= (liftIO . putStrLn . show)
                logList   v = mapM deRefVal v >>= (liftIO . putStrLn . show . map showJSValue)

            -- Add JavaScript logText function that calls the haskell logText
            (global <# "logText") (fun (\_f _this [s] -> logText s))
            -- jsLogText <- jsg "logText"

            -- logText("Hello World")
            (#) global "logText" ["Hello World"]

            -- console.log(Math.sin(1))
            math <- jsg "Math"
            let sin = js1 "sin"
            math ^. sin (1::Double) >>= logNumber

            -- (new Date()).toString()
            -- (new Date(2013,1,1)).toString()
            date <- jsg "Date"
            new date () >>= logText
            new date [2013,1,1::Double] >>= logText

            -- eval("logText('Hello'); 1+2")
            eval "logText('Hello'); 1+2" >>= log

            -- logText(["Test", navigator.appVersion].length)
            navigator  <- jsg "navigator"
            let appVersion = js "appVersion"
                jsLength   = js "length"
            (#) global "logText" (array ("Test", navigator ^. appVersion) ^. jsLength)

            -- callbackToHaskell = function () { console.log(arguments); }
            callBack <- (global <# "callbackToHaskell") (fun (\f this -> logList))

            -- callbackToHaskell(null, undefined, true, 3.14, "Hello")
            (#) global "callBackToHaskell" [ValNull, ValUndefined, ValBool True, ValNumber 3.14, ValString $ T.pack "List of JSValues"]
            -- or
            (#) global "callBackToHaskell" [val JSNull, val (), val True, val (3.14 :: Double), val "List of JSC JSValueRefs"]
            -- or
            (#) global "callBackToHaskell" (JSNull, (), True, (3.14 :: Double), "5-tuple")
            -- or
            eval "callbackToHaskell(null, undefined, true, 3.14, \"Eval\")"
#ifdef MIN_VERSION_jmacro
            -- or
            eval $(litE . stringL . show $ renderJs [jmacro|callbackToHaskell(null, undefined, true, 3.14, "Evaled JMacro")|])
            -- or
            jmfunc <- eval $(litE . stringL . show $ renderJs [jmacroE| \ a b c d e -> callbackToHaskell(a, b, c, d, e) |])
            let callJM :: (JSNull, (), Bool, Double, String) -> JSM = call jmfunc jmfunc
            callJM (JSNull, (), True, 3.14, "Via JMacro Evaled Function")
#endif

            -- var a = []; for(var i = 0; i != 10; ++i) a[i] = i; console.log(a[5]);
            array ([0..10]::[Double]) !! 5 >>= log

            return ()

    -- What is this?
    on heading click $ do
      shiftIsPressed <- mouseShiftKey
      when shiftIsPressed . liftIO $ lazyLoad_freecell webView doc body

    return ()

-- Integer uses goog.math.Integer compiled to Javascript
isPrime :: Integer -> Bool
isPrime p = p > 1 && (all (\n -> p `mod` n /= 0)
                     $ takeWhile (\n -> n*n <= p) [2..])

validatePrimeMessage :: Integer -> Html
validatePrimeMessage p | isPrime p = [shamlet|$newline always
                                        <b>Yes</b>, #{p} is a prime|]
                       | otherwise = [shamlet|$newline always
                                        <b>No</b>, #{p} is not a prime|]

validatePrime :: Maybe String -> Text
validatePrime ms =
    let s = fromMaybe "" ms
    in renderHtml $
      case reads s of
        [(n, "")] -> validatePrimeMessage n
        _         -> [shamlet|$newline always
                        <b>No</b>, that is not a number|]
-- Sometimes you might have something that needs more JavaScript than everything else
-- you can tell the GHCJS linker to put its dependancies in a sparate file using
-- a lazyLoad_ prefix
{-# NOINLINE lazyLoad_freecell #-}
lazyLoad_freecell webView doc body = do
    setInnerHTML body $
      (Just ("<div style=\"position:relative;left:0px;top:0px;background-color:#e0d0ff;width:700px;height:500px\" "
             ++ "id=\"freecell\" draggable=\"false\"></div>"))
    unlisten <- engine webView "freecell" =<< mkFreecell
    -- Prevent finalizers running too soon
    forkIO $ forever (threadDelay 1000000000) >> unlisten
    return ()

showJSValue :: JSValue -> String
showJSValue ValNull = "ValNull"
showJSValue ValUndefined = "ValUndefined"
showJSValue (ValBool b) = "ValBool " ++ show b
showJSValue (ValNumber n) = "ValNumber " ++ show n
showJSValue (ValString t) = "ValString " ++ show t
showJSValue (ValObject obj) = "ValObject (Object <JSObjectRef>)"
