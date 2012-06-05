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
import Paths_ghcjs_hello (getBinDir)
import Generator.Minify (minify, defaultMinify)
import System.Environment (getArgs)

main = do
    args <- getArgs
    binDir <- getBinDir
    let jsexe = binDir </> "ghcjs-hello-js.jsexe"
    writeFile (jsexe </> "hello.js") $ renderJavascript $ [julius|
            var link = function(f, inputId, outputId) {
                var input = document.getElementById(inputId);
                var setup = function() {
                    var run = function() {
                        $hs_fromLazyText([f, $hs_toLazyText(input.value)],
                            function(result) {
                                document.getElementById(outputId).innerHTML = result;});
                    }
                    input.onkeydown = run;
                    input.onkeyup = run;
                    input.onchange = run;
                    run();
                }
                input.onkeydown = setup;
                input.onkeyup = setup;
                input.onchange = setup;
            };
            var linkIO = function(f, inputId, outputId) {
                var input = document.getElementById(inputId);
                var setup = function() {
                    var run = function() {
                        $hs_runIO([f, input.value], function(str) {
                            $hs_fromLazyText([str],
                                function(result) {
                                    document.getElementById(outputId).innerHTML = result;});});
                              }
                    input.onkeydown = run;
                    input.onkeyup = run;
                    input.onchange = run;
                    run();
                }
                input.onkeydown = setup;
                input.onkeyup = setup;
                input.onchange = setup;
            };
            window.onload = function(){
                goog.debug.Console.autoInstall();
                $hs_loadPath = "./";

                // Must be called first
                $hs_init();

                link  ($$$Main_hello,         "name",     "hello");
                link  ($$$Main_validatePrime, "num",      "prime");
                linkIO($$$Main_tryHamlet,     "hamletIn", "hamletOut");
            }
        |] ()

    writeFile (jsexe </> "index.html") $ renderHtml [shamlet|
      <html>
        <head>
            <script type="text/javascript" src="rts.js">
            <script type="text/javascript" src="main.js">
            <title>GHCJS Examples
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
            <h1>GHCJS Examples
            Hi, what is your name?
            <input #name size="16">
            <div #hello>
            <p>Know any good prime numbers?
            <input #num size="8">
            <div #prime>
            <p>Have you tried <a href="http://hackage.haskell.org/package/hamlet-1.0.1.2">hamlet</a>?
            <textarea #hamletIn rows="20" cols="120">
            <div #hamletOut>
            <div #log>
        |]

