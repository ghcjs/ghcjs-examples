# Reactive Tabs Example

Code from the tutorial by @geraldus:
https://github.com/kentuckyfriedtakahe/sodium/issues/8

To build, run:

```
git clone https://github.com/ghcjs/ghcjs-jquery.git
cabal sandbox init ghcjs
cabal sandbox add-source ./ghcjs-jquery
cabal sandbox install --ghcjs --only-dependencies
cabal sandbox configure --ghcjs
cabal sandbox build
./prepare.sh
```
