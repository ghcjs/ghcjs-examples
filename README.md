GHCJS Examples
==============

These examples demonstrate some of the features of GHCJS.  Click on the links below to run
the examples in your browser.

[GHCJS Hello](http://ghcjs.github.com/bin/ghcjs-hello.trampoline.jsexe/)
* Lazy Loading
* Stdout and Stdin
* Threading and MVars
* Calling JavaScript from Haskell
* Calling back into Haskell from JavaScript
* Embedding JMacro code
* Using Hamlet
* Canvas

[Freecell](http://ghcjs.github.com/bin/freecell.trampoline.jsexe/)
* FRP in JavaScript

[Multiple Pages](http://ghcjs.github.com/share/multiple-pages-0.0.1/)
* Adding GHCJS to an existing website
* Lazy Loading

Compiled to Native with GHC and WebKitGTK
-----------------------------------------

[![Build Status](https://travis-ci.org/ghcjs/ghcjs-examples.png?branch=master)](https://travis-ci.org/ghcjs/ghcjs-examples)

To build and run these examples using WebKitGTK+ then do the following

    sudo apt-get install libwebkitgtk-3.0-dev
    mkdir vendor
    cd vendor
    darcs get --lazy http://patch-tag.com/r/hamish/gtk2hs
    cabal install ./gtk2hs/tools
    cd ..
    cabal install cabal-meta cabal-src
    cabal-meta install -fgtk3 --force-reinstalls
    
Due to an issue with gtk2hsC2hs you may have to run that last step twice.
You may also need to add -fwebkit1-8 if you have an older version of webkit.

Once this is done you should run the examples with 
    ghcjs-hello
    freecell
    multiple-pages [URL to share/multiple-pages-0.0.1/index.html]
    
Installing WebKitGTK+ is not well supported on OS X and Windows, so we
strongly recommend using Linux (or a Linux VM).

Compiled to JavaScript with Integrated GHCJS
--------------------------------------------
Follow the instructions in Integrated section of [GHCJS](https://github.com/ghcjs/ghcjs)

Compiled to JavaScript with Stand Alone GHCJS
---------------------------------------------
TODO Add instructions....

