#!/bin/bash

# This code is probably not directly useful to anyone else because of the hardcoded
# paths, but it shows how we build the binaries we put on our website...

PRODDIR=/home/andrew/Documents/lanthaps_consulting/products/prodtimer
BUILD32_ROOT=/data/build-server-lxc/root

(cabal configure --flags="Static" &&
 cabal build && ./installer/package-linux.sh &&
 cp /tmp/lanthaps_package/lanthaps-prodtimer-linux-installer.bin $PRODDIR/lanthaps-prodtimer-linux-installer-64.bin
# Disabled for the Open Source version since there is no LGPL requirement for
# a dynamic build.
# && cabal configure --flags="-Static" &&
# cabal build &&
# ./installer/package-linux.sh &&
# cp /tmp/lanthaps_package/lanthaps-prodtimer-linux-installer.bin $PRODDIR/lanthaps-prodtimer-linux-installer-dyn-64.bin
)&
(
 find /home/andrew/.wine/drive_c/ghc/ghc-7.0.4/lib/ -iname "*.dll" -print0 | xargs -0 -I{} ln -s "{}" ~/.wine/drive_c/windows/system32/ 2>/dev/null;
 find /home/andrew/.wine/drive_c/Program\ Files/Haskell/ -iname "*.dll" -print0 | xargs -0 -I{} ln -s "{}" ~/.wine/drive_c/windows/system32/ 2>/dev/null;
 rm /home/andrew/.wine/drive_c/windows/system32/libHSrts-ghc7.0.4.dll;
 # http://hackage.haskell.org/trac/ghc/ticket/5620
 ln -s /home/andrew/.wine/drive_c/ghc/ghc-7.0.4/lib/libHSrts_thr-ghc7.0.4.dll /home/andrew/.wine/drive_c/windows/system32/libHSrts-ghc7.0.4.dll &&
 cd ~/Documents/openprodtimer-win32 &&
 git pull &&
 wine cabal configure --flags="Static" &&
 wine cabal build &&
 ./installer/package-win32.sh &&
 cp /tmp/lanthaps_package/install-prodtimer.exe $PRODDIR/install-prodtimer.exe
# && wine cabal configure --flags="-Static" &&
# wine cabal build &&
# ./installer/package-win32.sh && cp /tmp/lanthaps_package/install-prodtimer.exe $PRODDIR/install-prodtimer-dyn.exe
)&
(ssh -X root@10.1.2.7 'cd /usr/src/openprodtimer && git pull && cabal configure --flags="Static" && cabal build && ./installer/package-linux.sh' &&
 cp $BUILD32_ROOT/tmp/lanthaps_package/lanthaps-prodtimer-linux-installer.bin $PRODDIR/lanthaps-prodtimer-linux-installer-32.bin
# && ssh -X root@10.1.2.7 'cd /usr/src/openprodtimer && cabal configure --flags="-Static" && cabal build && ./installer/package-linux.sh' &&
#  cp $BUILD32_ROOT/tmp/lanthaps_package/lanthaps-prodtimer-linux-installer.bin $PRODDIR/lanthaps-prodtimer-linux-installer-dyn-32.bin
)&
wait
