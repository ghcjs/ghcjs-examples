#!/bin/bash
( cd hello && ghcjs -o hello hello.hs )
( cd ffi && ghcjs -o ffi ffi.hs )
( cd fibonacci && ghcjs -O2 -o fibonacci fibonacci.hs)
( cd race && ghcjs -o race race.hs && cp race.html race.jsexe/index.html )
( cd event && ghcjs -o event event.hs && cp event.html event.jsexe/index.html )
( cd sync && ghcjs -o sync sync.hs && cp sync.html sync.jsexe/index.html )

