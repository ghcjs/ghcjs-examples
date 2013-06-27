#!/bin/bash

# http://weblog.luite.com/wordpress/?p=14

( cd hello && ghcjs -o hello hello.hs )
( cd ffi && ghcjs -o ffi ffi.hs )
( cd fibonacci && ghcjs -O2 -o fibonacci fibonacci.hs)
( cd race && ghcjs -o race race.hs && cp race.html race.jsexe/index.html )
( cd event && ghcjs -o event event.hs && cp event.html event.jsexe/index.html )
( cd sync && ghcjs -o sync sync.hs && cp sync.html sync.jsexe/index.html )

# http://weblog.luite.com/wordpress/?p=127
cabal install --ghcjs sodium random vector-space

( cd counter && ghcjs -o counter counter.hs && cp counter.html counter.jsexe/index.html )
( cd calculator && ghcjs -o calculator calculator.hs && cp calculator.html calculator.jsexe/index.html )
( cd mouse && ghcjs -o mouse mouse.hs && cp mouse.html mouse.jsexe/index.html )
( cd balls1 && ghcjs -O2 -o balls1 balls1.hs && cp balls1.html balls1.jsexe/index.html && cp ball.png balls1.jsexe )
( cd balls2 && ghcjs -O2 -o balls2 balls2.hs && cp balls2.html balls2.jsexe/index.html && cp ball.png balls2.jsexe )

