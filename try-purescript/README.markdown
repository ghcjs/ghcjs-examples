# Try PureScript!

Try PureScript! is an example that demonstrates the [ghcjs-ffiqq](https://github.com/ghcjs/ghcjs-ffiqq)
library and shows how long-running background computations can be done in asynchronous threads,
keeping the user interface responsive without the added complexity of web workers.

[PureScript](http://www.purescript.org/) is statically typed language that compiles
to JavaScript. Try PureScript! runs the PureScript compiler in the browser,
it includes a simplified prelude.

### Installation

Since Cabal support for web dependencies is not yet complete, you need to run a separate
script to copy them into the jsexe directory:

    cabal install --ghcjs --only-dependencies
    cabal configure --ghcjs
    cabal build
    prepare.sh

`prepare.sh` requires the tools described in the
[Deployment wiki](https://github.com/ghcjs/ghcjs/wiki/Deployment) to be installed. It minifies
the scripts, but `ADVANCED_OPTIMIZATIONS` (full renaming and dead code elimination) is currently
disabled due to some problems with it. It should be enabled again when these have been fixed.

to test locally with `warp-static`:

    cd dist/build/try-purescript/try-purescript.jsexe
    warp

Edit `dist/build/try-purescript/try-purescript.jsexe/index.html` to load `all.js`
instead of `all.min.js` for development. Then to rebuild, run:

    cabal build

