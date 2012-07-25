{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main (
    main
) where

import Prelude hiding (writeFile)
import Text.Julius (julius, renderJavascript)
import Text.Hamlet (defaultHamletSettings, shamlet, Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet.RT (renderHamletRT, parseHamletRT)
import Data.Text.Lazy.IO (writeFile)
import System.FilePath ((</>))
import Paths_fay_hello_js (getBinDir)
import Generator.Minify (minify, defaultMinify)
import System.Environment (getArgs)
import Control.Applicative ((<$>))

main = do
    args <- getArgs
    jsexe <- (</> "fay-hello-js.jsexe") <$> getBinDir
    writeFile (jsexe </> "hello.js") $ renderJavascript $ [julius|
            window.onload = function(){
                goog.debug.Console.autoInstall();
                $hs_loadPath = "./";

                // Must be called first
                $hs_init();

                var input = document.getElementById("fayIn");
                var autoRun = document.getElementById("autoRun");
                var compile = document.getElementById("compile");
                var output = document.getElementById("fayOut");
                compile.onclick = function() {
                    $hs_runIO([
                        $$$Main_compileFromTo,
                        $hs_mkBool(autoRun.checked),
                        $hs_toString(input.value)
                        ],
                        function(str) {
                            $hs_fromString([str],
                                function(result) {
                                    output.value = result;});});
                }
            }
        |] ()

    writeFile (jsexe </> "index.html") $ renderHtml [shamlet|
      <html>
        <head>
            <script type="text/javascript" src="rts.js">
            <script type="text/javascript" src="main.js">
            <title>GHCJS Examples - Fay
        #{body}|]

    writeFile (jsexe </> "index_raw.html") $ renderHtml [shamlet|
      <html>
        <head>
            <script type="text/javascript" src="../closure-library/closure/goog/base.js">
            <script type="text/javascript">
                goog.require('goog.math.Long');
                goog.require('goog.math.Integer');
                goog.require('goog.debug.Logger');
                goog.require('goog.debug.Console');
            <script type="text/javascript" src="../rts/rts-options.js">
            <script type="text/javascript" src="../rts/rts-common.js">
            <script type="text/javascript" src="../rts/rts-trampoline.js">
            <script type="text/javascript" src="hsloader.js">
            <script type="text/javascript" src="hello.js">
            <title>GHCJS Examples
        #{body}|]

    minify jsexe [jsexe </> "hello.js"] defaultMinify args
  where
    body = [shamlet|
        <body #slideBody>
            <textarea #fayIn rows="20" cols="120">
            <p>
                <input #autoRun type="checkbox">Include auto run code
                <button #compile>Compile Fay
            <textarea #fayOut rows="20" cols="120">
            <div #log>
        |]

