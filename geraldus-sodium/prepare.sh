#!/bin/sh

# run this script after building the jsexe to copy data files
# this task really ought to be done with Cabal, but it doesn't support this yet

set -e

DISTDIR="dist"
EXEDIR="$DISTDIR/build/geraldus-sodium/geraldus-sodium.jsexe"

cp "index.html"     "$EXEDIR/index.html"
cp "style.css"      "$EXEDIR/style.css"
