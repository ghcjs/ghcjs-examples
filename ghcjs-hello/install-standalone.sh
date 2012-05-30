#!/bin/sh
cabal install
(
  cd js
  ghcjs-cabal install
)

