#!/bin/sh -ex
sudo apt-get update -qq
sudo apt-get --no-install-recommends install darcs libwebkitgtk-3.0-dev
git clone https://github.com/yesodweb/cabal-meta.git
darcs get --lazy http://patch-tag.com/r/hamish/gtk2hs
cabal install ./gtk2hs/tools

