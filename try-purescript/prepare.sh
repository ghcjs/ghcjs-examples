#!/bin/sh

# run this script after building the jsexe to copy data files and minify the sources
# assumes that the tools described in the Deployment wiki page are installed.
#   https://github.com/ghcjs/ghcjs/wiki/Deployment
#
# this task really ought to be done with Cabal, but it doesn't support this yet

set -e

DISTDIR="dist"
EXEDIR="$DISTDIR/build/try-purescript/try-purescript.jsexe"

cp "data/index.html"     "$EXEDIR/index.html"
cp "data/tryps.css"      "$EXEDIR/tryps.css"
cp "data/run_button.png" "$EXEDIR/run_button.png"
cp "data/busy.gif"       "$EXEDIR/busy.gif"

cp "codemirror/lib/codemirror.js" "$EXEDIR/codemirror.js"
cat "codemirror/addon/edit/matchbrackets.js" >> "$EXEDIR/codemirror.js"
cat "codemirror/mode/haskell/haskell.js" >> "$EXEDIR/codemirror.js"

cp "codemirror/lib/codemirror.css" "$EXEDIR/codemirror.css"
cat "codemirror/theme/elegant.css" >> "$EXEDIR/codemirror.css"

(
    cd "$EXEDIR"
    # advanced optimizations are currently buggy for this program, enable them again when fixed
    echo "minifying all.js"
    ccjs "all.js" > "all.min.js"
    echo "compressing all.min.js"
    zopfli -i1000 "all.min.js"
    echo "minifying codemirror.js"
    ccjs "codemirror.js" > "codemirror.min.js"
    echo "compressing codemirror.min.js"
    zopfli -i1000 "codemirror.min.js"
)

